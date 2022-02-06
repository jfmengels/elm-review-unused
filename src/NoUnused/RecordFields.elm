module NoUnused.RecordFields exposing (rule)

{-|

@docs rule

-}

import Array exposing (Array)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import NoUnused.RecordFields.RangeSet as RangeSet exposing (RangeSet)
import NoUnused.RecordFields.Variable as Variable exposing (Variable)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import TypeInference
import TypeInference.Type as Type exposing (Type)
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
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


type alias ProjectContext =
    { typeInference : TypeInference.ProjectContext
    }


type alias ModuleContext =
    { moduleNameLookupTable : ModuleNameLookupTable
    , typeByNameLookup : TypeByNameLookup
    , typeInference : TypeInference.ModuleContext
    , variableRegister : Variable.Register
    , directAccessesToIgnore : RangeSet
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
            , variableRegister = Variable.emptyRegister
            , directAccessesToIgnore = RangeSet.empty
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
                variables : List ( String, Variable )
                variables =
                    nodes
                        |> List.filterMap (registerDeclaration exposedNames)
            in
            ( [], { context | variableRegister = Variable.addVariables variables context.variableRegister } )


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
    case Maybe.map (Node.value >> .typeAnnotation) function.signature of
        Just typeAnnotation ->
            case returnType typeAnnotation of
                NoActionableReturnType ->
                    Nothing

                LiteralRecord ->
                    findDeclarationFields declaration.expression
                        |> Maybe.map (\declaredFields -> ( name, declaredFields ))

        Nothing ->
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
            handleDeclaration { moduleContext | directAccessesToIgnore = RangeSet.empty } function

        _ ->
            ( [], moduleContext )


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
                | variableRegister = Variable.addVariables variables moduleContext.variableRegister
              }
            )

        _ ->
            ( [], moduleContext )


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



-- EXPRESSION ENTER VISITOR


type ArgumentMatchResult
    = ArgumentMatch_RegisterVariable { variableName : String, declaredFields : List String, variableExpressionToIgnore : Range }
    | ArgumentMatch_VariableExpressionToIgnore Range
    | ArgumentMatch_ReportErrors (List (Node String))


extractOutOfArgumentMatchResults : List ArgumentMatchResult -> { errors : List (Error {}), variables : List { variableName : String, declaredFields : List String }, variableExpressionToIgnore : List Range }
extractOutOfArgumentMatchResults argumentMatchResults =
    List.foldl
        (\argumentMatch acc ->
            case argumentMatch of
                ArgumentMatch_RegisterVariable { variableName, declaredFields, variableExpressionToIgnore } ->
                    { acc
                        | variables = { variableName = variableName, declaredFields = declaredFields } :: acc.variables
                        , variableExpressionToIgnore = variableExpressionToIgnore :: acc.variableExpressionToIgnore
                    }

                ArgumentMatch_VariableExpressionToIgnore range ->
                    { acc | variableExpressionToIgnore = range :: acc.variableExpressionToIgnore }

                ArgumentMatch_ReportErrors unusedFieldNodes ->
                    { acc | errors = List.map createError unusedFieldNodes ++ acc.errors }
        )
        { errors = [], variables = [], variableExpressionToIgnore = [] }
        argumentMatchResults


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.RecordAccess (Node functionOrValueRange (Expression.FunctionOrValue [] name)) fieldName ->
            let
                newContext : ModuleContext
                newContext =
                    updateRegister name (Variable.markFieldAsUsed (Node.value fieldName)) context
            in
            ( []
            , { newContext | directAccessesToIgnore = RangeSet.insert functionOrValueRange context.directAccessesToIgnore }
            )

        Expression.RecordAccess _ _ ->
            ( [], context )

        Expression.Application (function :: arguments) ->
            case TypeInference.inferType context function of
                Just (Type.Function input output) ->
                    let
                        argumentTypes : List Type
                        argumentTypes =
                            input :: getListOfArgumentTypes output

                        allElements : Array Type
                        allElements =
                            input
                                :: argumentsAndReturnType output
                                |> Array.fromList

                        removeAt : Int -> Array a -> List a
                        removeAt index array =
                            Array.append
                                (Array.slice 0 index array)
                                (Array.slice (index + 1) (Array.length array) array)
                                |> Array.toList

                        argumentMatchResults : List ArgumentMatchResult
                        argumentMatchResults =
                            List.map2 Tuple.pair argumentTypes arguments
                                |> List.indexedMap
                                    (\index ( type_, argument ) ->
                                        case type_ of
                                            Type.Record { fields } ->
                                                matchRecordWithArgument fields argument

                                            Type.Generic generic ->
                                                if List.any (isGenericUsed generic) (removeAt index allElements) then
                                                    Nothing

                                                else
                                                    Just (ArgumentMatch_VariableExpressionToIgnore (Node.range argument))

                                            _ ->
                                                Nothing
                                    )
                                |> List.filterMap identity

                        summarizedArgumentMatchResults :
                            { errors : List (Error {})
                            , variables : List { variableName : String, declaredFields : List String }
                            , variableExpressionToIgnore : List Range
                            }
                        summarizedArgumentMatchResults =
                            extractOutOfArgumentMatchResults argumentMatchResults

                        variableExpressionToIgnore : RangeSet
                        variableExpressionToIgnore =
                            RangeSet.fromList summarizedArgumentMatchResults.variableExpressionToIgnore

                        newContext : ModuleContext
                        newContext =
                            List.foldl
                                (\{ variableName, declaredFields } ctx ->
                                    updateRegister variableName (Variable.markFieldsAsUsed declaredFields) ctx
                                )
                                { context | directAccessesToIgnore = RangeSet.union variableExpressionToIgnore context.directAccessesToIgnore }
                                summarizedArgumentMatchResults.variables
                    in
                    ( summarizedArgumentMatchResults.errors, newContext )

                _ ->
                    ( [], context )

        Expression.Application [] ->
            ( [], context )

        Expression.OperatorApplication operator _ left right ->
            --case TypeInference.inferOperatorType context operator of
            --    Just ((Type.Function _ _) as functionType) ->
            --        let
            --            getArgumentsFromType : Type -> List Type
            --            getArgumentsFromType type_ =
            --                case type_ of
            --                    Type.Function input output ->
            --                        input :: getArgumentsFromType output
            --
            --                    _ ->
            --                        []
            --
            --            argumentMatchResults : List Foo
            --            argumentMatchResults =
            --                List.map2
            --                    (\type_ argument ->
            --                        case ( type_, argument ) of
            --                            ( Type.Record { fields }, Node functionOrValueRange (Expression.FunctionOrValue [] name) ) ->
            --                                Just (Foo_Variable { variableName = name, declaredFields = List.map Tuple.first fields, variableExpressionToIgnore = functionOrValueRange })
            --
            --                            ( Type.Record { fields }, Node _ (Expression.RecordExpr recordSetters) ) ->
            --                                let
            --                                    usedFields : Set String
            --                                    usedFields =
            --                                        List.map Tuple.first fields
            --                                            |> Set.fromList
            --                                in
            --                                recordSetters
            --                                    |> List.map Node.value
            --                                    |> List.map Tuple.first
            --                                    |> List.filter (\declaredField -> not (Set.member (Node.value declaredField) usedFields))
            --                                    |> Foo_Errors
            --                                    |> Just
            --
            --                            _ ->
            --                                Nothing
            --                    )
            --                    (getArgumentsFromType functionType)
            --                    [ left, right ]
            --                    |> List.filterMap identity
            --
            --            ( errors, foundUsedFields ) =
            --                extractOutOfArgumentMatchResults argumentMatchResults
            --
            --            newContext : ModuleContext
            --            newContext =
            --                List.foldl
            --                    (\{ variableName, declaredFields, variableExpressionToIgnore } ctx ->
            --                        { ctx | directAccessesToIgnore = Set.insert (stringifyRange variableExpressionToIgnore) ctx.directAccessesToIgnore }
            --                            |> updateRegister variableName (Variable.markFieldsAsUsed declaredFields)
            --                    )
            --                    context
            --                    foundUsedFields
            --        in
            --        ( errors, newContext )
            --
            --    _ ->
            ( [], context )

        Expression.FunctionOrValue [] name ->
            if RangeSet.member (Node.range node) context.directAccessesToIgnore then
                ( [], updateRegister name Variable.markAsUsed context )

            else
                ( [], updateRegister name Variable.markAsUsedInAnUnknownManner context )

        Expression.FunctionOrValue _ name ->
            ( [], context )

        Expression.RecordUpdateExpression name _ ->
            ( [], updateRegister (Node.value name) Variable.markAsUsedInAnUnknownManner context )

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
            , { context | variableRegister = Variable.addVariablesInNewScope variables context.variableRegister }
            )

        Expression.RecordAccessFunction _ ->
            ( [], context )

        Expression.RecordExpr nodes ->
            ( [], context )

        Expression.UnitExpr ->
            ( [], context )

        Expression.IfBlock _ _ _ ->
            ( [], context )

        Expression.PrefixOperator _ ->
            ( [], context )

        Expression.Operator _ ->
            ( [], context )

        Expression.Integer _ ->
            ( [], context )

        Expression.Hex _ ->
            ( [], context )

        Expression.Floatable float ->
            ( [], context )

        Expression.Negation _ ->
            ( [], context )

        Expression.Literal _ ->
            ( [], context )

        Expression.CharLiteral _ ->
            ( [], context )

        Expression.TupledExpression _ ->
            ( [], context )

        Expression.ParenthesizedExpression _ ->
            ( [], context )

        Expression.CaseExpression _ ->
            ( [], context )

        Expression.LambdaExpression _ ->
            ( [], context )

        Expression.ListExpr _ ->
            ( [], context )

        Expression.GLSLExpression _ ->
            ( [], context )


isGenericUsed : String -> Type -> Bool
isGenericUsed genericToFind type_ =
    case type_ of
        Type.Generic generic ->
            genericToFind == generic

        Type.Unknown ->
            False

        Type.Function input output ->
            False

        Type.Tuple types ->
            False

        Type.Type moduleName string types ->
            False

        Type.Record record ->
            False


getListOfArgumentTypes : Type -> List Type
getListOfArgumentTypes type_ =
    case type_ of
        Type.Function input output ->
            input :: getListOfArgumentTypes output

        _ ->
            []


argumentsAndReturnType : Type -> List Type
argumentsAndReturnType type_ =
    case type_ of
        Type.Function input output ->
            input :: argumentsAndReturnType output

        _ ->
            [ type_ ]


matchRecordWithArgument : List ( String, a ) -> Node Expression -> Maybe ArgumentMatchResult
matchRecordWithArgument fields node =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            Just
                (ArgumentMatch_RegisterVariable
                    { variableName = name
                    , declaredFields = List.map Tuple.first fields
                    , variableExpressionToIgnore = Node.range node
                    }
                )

        Expression.RecordExpr recordSetters ->
            let
                usedFields : Set String
                usedFields =
                    List.map Tuple.first fields
                        |> Set.fromList
            in
            recordSetters
                |> List.map Node.value
                |> List.map Tuple.first
                |> List.filter (\declaredField -> not (Set.member (Node.value declaredField) usedFields))
                |> ArgumentMatch_ReportErrors
                |> Just

        Expression.ParenthesizedExpression expr ->
            matchRecordWithArgument fields expr

        _ ->
            Nothing



-- EXPRESSION EXIT VISITOR


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression _ ->
            let
                ( unusedDeclaredFields, variableRegister ) =
                    Variable.unusedDeclaredFieldsForScope context.variableRegister
            in
            ( List.map createError unusedDeclaredFields, { context | variableRegister = variableRegister } )

        _ ->
            ( [], context )


updateRegister : String -> (Variable -> Variable) -> ModuleContext -> ModuleContext
updateRegister name func context =
    { context | variableRegister = Variable.updateVariable name func context.variableRegister }



-- FINAL EVALUATION


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation context =
    Variable.unusedDeclaredFieldsForScope context.variableRegister
        |> Tuple.first
        |> List.map createError


createError : Node String -> Error {}
createError unusedFieldNode =
    Rule.error
        { message = "Unused field `" ++ Node.value unusedFieldNode ++ "`"
        , details = [ "This field has been declared and may have been assigned to, but is never used. You may have forgotten to use it where needed. Please do so or remove the field." ]
        }
        (Node.range unusedFieldNode)
