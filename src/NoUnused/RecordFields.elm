module NoUnused.RecordFields exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import TypeInference
import TypeInference.TypeByNameLookup as TypeByNameLookup exposing (TypeByNameLookup)


{-| Reports... REPLACEME

    config =
        [ NoUnused.RecordFields.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.RecordFields
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.RecordFields" initialContext
        |> TypeInference.addProjectVisitors
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


type alias ProjectContext =
    { typeInference : TypeInference.ProjectContext
    }


type alias ModuleContext =
    { moduleNameLookupTable : ModuleNameLookupTable
    , typeByNameLookup : TypeByNameLookup
    , typeInference : TypeInference.ModuleContext
    , variables : Dict String Variable
    , directAccessesToIgnore : Set String
    , exposes : Exposes
    }


type alias Variable =
    { usedFields : Set String
    , declaredFields : List (Node String)
    , wasUsed : Bool
    , wasUsedInAnUnknownManner : Bool
    }


initialContext : ProjectContext
initialContext =
    { typeInference = TypeInference.initialProjectContext
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\moduleNameLookupTable projectContext ->
            { typeInference = TypeInference.fromProjectToModule projectContext
            , moduleNameLookupTable = moduleNameLookupTable
            , variables = Dict.empty
            , directAccessesToIgnore = Set.empty
            , exposes = ExposesEverything
            , typeByNameLookup = TypeByNameLookup.empty
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { typeInference =
                TypeInference.fromModuleToProject
                    (Rule.moduleNameFromMetadata metadata)
                    moduleContext.typeInference
            }
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { typeInference = TypeInference.foldProjectContexts newContext.typeInference previousContext.typeInference
    }


type Exposes
    = ExposesEverything
    | ExposesExplicitly (Set String)


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor moduleNode moduleContext =
    case Module.exposingList (Node.value moduleNode) of
        Exposing.All _ ->
            ( [], { moduleContext | exposes = ExposesEverything } )

        Exposing.Explicit list ->
            let
                names : List String
                names =
                    List.map
                        (\(Node _ node) ->
                            case node of
                                Exposing.FunctionExpose name ->
                                    name

                                Exposing.TypeOrAliasExpose name ->
                                    name

                                Exposing.TypeExpose { name } ->
                                    name

                                Exposing.InfixExpose name ->
                                    name
                        )
                        list
            in
            ( [], { moduleContext | exposes = ExposesExplicitly (Set.fromList names) } )


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor nodes context =
    let
        variables : Dict String Variable
        variables =
            nodes
                |> List.filterMap (registerDeclaration context.exposes)
                |> Dict.fromList
    in
    ( [], { context | variables = Dict.union variables context.variables } )


registerDeclaration : Exposes -> Node Declaration -> Maybe ( String, Variable )
registerDeclaration exposes node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            declarationFields exposes function
                |> Maybe.map (\( name, declaredFields ) -> ( name, createVariable declaredFields Set.empty ))

        _ ->
            Nothing


createVariable : List (Node String) -> Set String -> Variable
createVariable declaredFields usedFields =
    { usedFields = usedFields
    , declaredFields = declaredFields
    , wasUsed = False
    , wasUsedInAnUnknownManner = False
    }


declarationFields :
    Exposes
    -> { a | signature : Maybe (Node Signature), declaration : Node Expression.FunctionImplementation }
    -> Maybe ( String, List (Node String) )
declarationFields exposes function =
    case exposes of
        ExposesEverything ->
            Nothing

        ExposesExplicitly exposedNames ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration

                name : String
                name =
                    Node.value declaration.name
            in
            if not (Set.member name exposedNames) && List.isEmpty declaration.arguments then
                case Maybe.map (Node.value >> .typeAnnotation >> returnType) function.signature of
                    Just NoActionableReturnType ->
                        Nothing

                    Nothing ->
                        findDeclarationFields declaration.expression
                            |> Maybe.map (\declaredFields -> ( name, declaredFields ))

                    Just LiteralRecord ->
                        findDeclarationFields declaration.expression
                            |> Maybe.map (\declaredFields -> ( name, declaredFields ))

            else
                Nothing


findDeclarationFields : Node Expression -> Maybe (List (Node String))
findDeclarationFields expression =
    case Node.value expression of
        Expression.RecordExpr fields ->
            let
                declaredFields : List (Node String)
                declaredFields =
                    List.map (Node.value >> Tuple.first) fields
            in
            Just declaredFields

        _ ->
            Nothing


type ReturnType
    = NoActionableReturnType
    | LiteralRecord


returnType : Node TypeAnnotation -> ReturnType
returnType node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation _ output ->
            returnType output

        TypeAnnotation.Record _ ->
            LiteralRecord

        _ ->
            NoActionableReturnType


declarationEnterVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationEnterVisitor node moduleContext =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            handleDeclaration moduleContext function

        _ ->
            ( [], { moduleContext | directAccessesToIgnore = Set.empty } )


handleDeclaration : ModuleContext -> Expression.Function -> ( List (Error {}), ModuleContext )
handleDeclaration moduleContext { signature, declaration } =
    case Maybe.map (Node.value >> .typeAnnotation) signature of
        Just typeAnnotation ->
            let
                recordArguments : List (Maybe (List (Node String)))
                recordArguments =
                    recordDefinitionsFromTypeAnnotation typeAnnotation

                arguments : List (Node Pattern)
                arguments =
                    (Node.value declaration).arguments

                variableOrErrors : List (Maybe VariableOrError)
                variableOrErrors =
                    List.map2
                        (\recordArgument argument ->
                            case recordArgument of
                                Just ((_ :: _) as declaredFields) ->
                                    createVariableOrErrors declaredFields argument

                                _ ->
                                    Nothing
                        )
                        recordArguments
                        arguments

                ( errorsToReport, variables ) =
                    getErrorsAndVariables variableOrErrors
            in
            ( errorsToReport
            , { moduleContext
                | variables = Dict.union (Dict.fromList variables) moduleContext.variables
                , directAccessesToIgnore = Set.empty
              }
            )

        _ ->
            ( [], { moduleContext | directAccessesToIgnore = Set.empty } )


getErrorsAndVariables : List (Maybe VariableOrError) -> ( List (Error {}), List ( String, Variable ) )
getErrorsAndVariables variableOrErrors =
    List.foldl
        (\variableOrError (( errors, variables ) as acc) ->
            case variableOrError of
                Just (VariableOrError_Variable variable) ->
                    ( errors, variable :: variables )

                Just (VariableOrError_Errors unusedFieldNodes) ->
                    let
                        newErrors : List (Error {})
                        newErrors =
                            List.map
                                (\unusedFieldNode ->
                                    Rule.error
                                        { message = "Unused field `" ++ Node.value unusedFieldNode ++ "`"
                                        , details = [ "REPLACEME" ]
                                        }
                                        (Node.range unusedFieldNode)
                                )
                                unusedFieldNodes
                    in
                    ( newErrors ++ errors, variables )

                Nothing ->
                    acc
        )
        ( [], [] )
        variableOrErrors


type VariableOrError
    = VariableOrError_Variable ( String, Variable )
    | VariableOrError_Errors (List (Node String))


createVariableOrErrors : List (Node String) -> Node Pattern -> Maybe VariableOrError
createVariableOrErrors declaredFields argument =
    case Node.value argument of
        Pattern.VarPattern name ->
            Just (VariableOrError_Variable ( name, createVariable declaredFields Set.empty ))

        Pattern.ParenthesizedPattern pattern ->
            createVariableOrErrors declaredFields pattern

        Pattern.AsPattern pattern name ->
            Just (VariableOrError_Variable ( Node.value name, createVariable declaredFields (Set.fromList (fieldsFromPattern pattern)) ))

        Pattern.RecordPattern nodes ->
            let
                destructuredNames : Set String
                destructuredNames =
                    List.map Node.value nodes
                        |> Set.fromList
            in
            declaredFields
                |> List.filter (\node -> not (Set.member (Node.value node) destructuredNames))
                |> VariableOrError_Errors
                |> Just

        _ ->
            Nothing


fieldsFromPattern : Node Pattern -> List String
fieldsFromPattern node =
    case Node.value node of
        Pattern.AllPattern ->
            []

        Pattern.UnitPattern ->
            []

        Pattern.CharPattern _ ->
            []

        Pattern.StringPattern _ ->
            []

        Pattern.IntPattern _ ->
            []

        Pattern.HexPattern _ ->
            []

        Pattern.FloatPattern _ ->
            []

        Pattern.TuplePattern _ ->
            -- TODO
            []

        Pattern.RecordPattern nodes ->
            List.map Node.value nodes

        Pattern.UnConsPattern _ _ ->
            -- TODO
            []

        Pattern.ListPattern _ ->
            -- TODO
            []

        Pattern.VarPattern _ ->
            []

        Pattern.NamedPattern _ _ ->
            []

        Pattern.AsPattern _ _ ->
            []

        Pattern.ParenthesizedPattern _ ->
            []


recordDefinitionsFromTypeAnnotation : Node TypeAnnotation -> List (Maybe (List (Node String)))
recordDefinitionsFromTypeAnnotation typeAnnotation =
    case Node.value typeAnnotation of
        TypeAnnotation.FunctionTypeAnnotation input output ->
            extractRecordDefinition input :: recordDefinitionsFromTypeAnnotation output

        _ ->
            []


extractRecordDefinition : Node TypeAnnotation -> Maybe (List (Node String))
extractRecordDefinition typeAnnotation =
    case Node.value typeAnnotation of
        TypeAnnotation.Record recordDefinition ->
            Just (List.map (Node.value >> Tuple.first) recordDefinition)

        TypeAnnotation.GenericRecord _ recordDefinition ->
            Just (List.map (Node.value >> Tuple.first) (Node.value recordDefinition))

        _ ->
            Nothing


updateVariable : String -> (Variable -> Variable) -> Dict String Variable -> Dict String Variable
updateVariable name function variables =
    Dict.update name (Maybe.map function) variables


expressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.RecordAccess (Node functionOrValueRange (Expression.FunctionOrValue [] name)) fieldName ->
            let
                variables : Dict String Variable
                variables =
                    updateVariable name
                        (\declared ->
                            { declared
                                | wasUsed = True
                                , usedFields = Set.insert (Node.value fieldName) declared.usedFields
                            }
                        )
                        context.variables
            in
            ( []
            , { context
                | variables = variables
                , directAccessesToIgnore = Set.insert (stringifyRange functionOrValueRange) context.directAccessesToIgnore
              }
            )

        Expression.FunctionOrValue [] name ->
            if Set.member (stringifyRange (Node.range node)) context.directAccessesToIgnore then
                let
                    variables : Dict String Variable
                    variables =
                        updateVariable name
                            (\declared -> { declared | wasUsed = True })
                            context.variables
                in
                ( [], { context | variables = variables } )

            else
                let
                    variables : Dict String Variable
                    variables =
                        updateVariable name
                            (\declared -> { declared | wasUsed = True, wasUsedInAnUnknownManner = True })
                            context.variables
                in
                ( [], { context | variables = variables } )

        Expression.RecordUpdateExpression name _ ->
            let
                variables : Dict String Variable
                variables =
                    updateVariable (Node.value name)
                        (\declared -> { declared | wasUsed = True, wasUsedInAnUnknownManner = True })
                        context.variables
            in
            ( [], { context | variables = variables } )

        Expression.LetExpression letBlock ->
            let
                variableOrErrors : List (Maybe VariableOrError)
                variableOrErrors =
                    letBlock.declarations
                        |> List.map
                            (\declaration ->
                                case Node.value declaration of
                                    Expression.LetFunction function ->
                                        declarationFields context.exposes function
                                            |> Maybe.map (\( name, declaredFields ) -> VariableOrError_Variable ( name, createVariable declaredFields Set.empty ))

                                    Expression.LetDestructuring pattern expression ->
                                        case Node.value pattern of
                                            Pattern.RecordPattern fields ->
                                                let
                                                    usedFields : Set String
                                                    usedFields =
                                                        List.map Node.value fields
                                                            |> Set.fromList
                                                in
                                                findDeclarationFields expression
                                                    |> Maybe.map
                                                        (\declaredFields ->
                                                            List.filter (\declaredField -> not (Set.member (Node.value declaredField) usedFields)) declaredFields
                                                        )
                                                    |> Maybe.map VariableOrError_Errors

                                            _ ->
                                                Nothing
                            )

                ( errorsToReport, variables ) =
                    getErrorsAndVariables variableOrErrors
            in
            ( errorsToReport
            , { context | variables = Dict.union (Dict.fromList variables) context.variables }
            )

        _ ->
            ( [], context )


stringifyRange : Range -> String
stringifyRange range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
        |> List.map String.fromInt
        |> String.join "-"


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation context =
    context.variables
        |> Dict.toList
        |> List.concatMap (Tuple.second >> finalEvaluationForVariable)


finalEvaluationForVariable : Variable -> List (Error {})
finalEvaluationForVariable variable =
    if variable.wasUsedInAnUnknownManner || not variable.wasUsed then
        []

    else
        variable.declaredFields
            |> List.filter (\node -> not <| Set.member (Node.value node) variable.usedFields)
            |> List.map
                (\node ->
                    Rule.error
                        { message = "Unused field `" ++ Node.value node ++ "`"
                        , details = [ "REPLACEME" ]
                        }
                        (Node.range node)
                )
