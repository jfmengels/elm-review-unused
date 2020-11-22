module NoUnused.RecordFields exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import NoUnused.RecordFields.Variable as Variable exposing (Variable)
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
    , variableRegister : Variable.Register
    , directAccessesToIgnore : Set String
    , exposes : Exposes
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
            , variableRegister = Dict.empty
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



-- MODULE DEFINITION VISITOR


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



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor nodes context =
    case context.exposes of
        ExposesEverything ->
            ( [], context )

        ExposesExplicitly exposedNames ->
            let
                variableRegister : Variable.Register
                variableRegister =
                    nodes
                        |> List.filterMap (registerDeclaration exposedNames)
                        |> Dict.fromList
            in
            ( [], { context | variableRegister = Dict.union variableRegister context.variableRegister } )


registerDeclaration : Set String -> Node Declaration -> Maybe ( String, Variable )
registerDeclaration exposedNames node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration

                name : String
                name =
                    Node.value declaration.name
            in
            if not (Set.member name exposedNames) && List.isEmpty declaration.arguments then
                declarationFields function
                    |> Maybe.map (\( name_, declaredFields ) -> ( name_, Variable.newVariable declaredFields Set.empty ))

            else
                Nothing

        _ ->
            Nothing


declarationFields :
    { a | signature : Maybe (Node Signature), declaration : Node Expression.FunctionImplementation }
    -> Maybe ( String, List (Node String) )
declarationFields function =
    let
        declaration : Expression.FunctionImplementation
        declaration =
            Node.value function.declaration

        name : String
        name =
            Node.value declaration.name
    in
    case Maybe.map (Node.value >> .typeAnnotation >> returnType) function.signature of
        Just NoActionableReturnType ->
            Nothing

        Nothing ->
            findDeclarationFields declaration.expression
                |> Maybe.map (\declaredFields -> ( name, declaredFields ))

        Just LiteralRecord ->
            findDeclarationFields declaration.expression
                |> Maybe.map (\declaredFields -> ( name, declaredFields ))


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



-- DECLARATION VISITOR


declarationEnterVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationEnterVisitor node moduleContext =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            handleDeclaration moduleContext function

        _ ->
            ( [], { moduleContext | directAccessesToIgnore = Set.empty } )


type VariableOrError
    = VariableOrError_Variable ( String, Variable )
    | VariableOrError_Errors (List (Node String))


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
                | variableRegister = Dict.union (Dict.fromList variables) moduleContext.variableRegister
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
                    ( List.map createError unusedFieldNodes ++ errors, variables )

                Nothing ->
                    acc
        )
        ( [], [] )
        variableOrErrors


createVariableOrErrors : List (Node String) -> Node Pattern -> Maybe VariableOrError
createVariableOrErrors declaredFields argument =
    case Node.value argument of
        Pattern.VarPattern name ->
            Just (VariableOrError_Variable ( name, Variable.newVariable declaredFields Set.empty ))

        Pattern.ParenthesizedPattern pattern ->
            createVariableOrErrors declaredFields pattern

        Pattern.AsPattern pattern name ->
            Just (VariableOrError_Variable ( Node.value name, Variable.newVariable declaredFields (Set.fromList (fieldsFromPattern pattern)) ))

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



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.RecordAccess (Node functionOrValueRange (Expression.FunctionOrValue [] name)) fieldName ->
            let
                variableRegister : Variable.Register
                variableRegister =
                    Variable.updateVariable name
                        (Variable.markFieldAsUsed (Node.value fieldName))
                        context.variableRegister
            in
            ( []
            , { context
                | variableRegister = variableRegister
                , directAccessesToIgnore = Set.insert (stringifyRange functionOrValueRange) context.directAccessesToIgnore
              }
            )

        Expression.FunctionOrValue [] name ->
            if Set.member (stringifyRange (Node.range node)) context.directAccessesToIgnore then
                let
                    variableRegister : Variable.Register
                    variableRegister =
                        Variable.updateVariable name
                            Variable.markAsUsed
                            context.variableRegister
                in
                ( [], { context | variableRegister = variableRegister } )

            else
                let
                    variableRegister : Variable.Register
                    variableRegister =
                        Variable.updateVariable name
                            Variable.markAsUsedInAnUnknownManner
                            context.variableRegister
                in
                ( [], { context | variableRegister = variableRegister } )

        Expression.RecordUpdateExpression name _ ->
            let
                variableRegister : Variable.Register
                variableRegister =
                    Variable.updateVariable (Node.value name)
                        Variable.markAsUsedInAnUnknownManner
                        context.variableRegister
            in
            ( [], { context | variableRegister = variableRegister } )

        Expression.LetExpression letBlock ->
            let
                variableOrErrors : List (Maybe VariableOrError)
                variableOrErrors =
                    letBlock.declarations
                        |> List.map
                            (\declaration ->
                                case Node.value declaration of
                                    Expression.LetFunction function ->
                                        declarationFields function
                                            |> Maybe.map (\( name, declaredFields ) -> VariableOrError_Variable ( name, Variable.newVariable declaredFields Set.empty ))

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
            , { context | variableRegister = Dict.union (Dict.fromList variables) context.variableRegister }
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
    Variable.unusedDeclaredFields context.variableRegister
        |> List.map createError


createError : Node String -> Error {}
createError unusedFieldNode =
    Rule.error
        { message = "Unused field `" ++ Node.value unusedFieldNode ++ "`"
        , details = [ "REPLACEME" ]
        }
        (Node.range unusedFieldNode)
