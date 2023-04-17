module NoUnused.AliasRecordFields exposing (rule)

{-|

@docs rule

-}

import Array exposing (Array)
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
        [ NoUnused.AliasRecordFields.rule
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
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.AliasRecordFields
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.AliasRecordFields" initialContext
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
        |> Rule.withModuleDefinitionVisitor (\node context -> ( [], moduleDefinitionVisitor node context ))
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


type alias ProjectContext =
    { typeInference : TypeInference.ProjectContext
    }


type alias ModuleContext =
    { directAccessesToIgnore : RangeSet
    , exposes : Exposes
    , moduleNameLookupTable : ModuleNameLookupTable
    , recordRegister : Variable.Register
    , typeByNameLookup : TypeByNameLookup
    , typeInference : TypeInference.ModuleContext
    , variableRecord : Dict String String
    , variableRegister : Variable.Register
    }


initialContext : ProjectContext
initialContext =
    { typeInference = TypeInference.initialProjectContext
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\moduleNameLookupTable projectContext ->
            { directAccessesToIgnore = RangeSet.empty
            , exposes = ExposesEverything
            , moduleNameLookupTable = moduleNameLookupTable
            , recordRegister = Variable.emptyRegister
            , typeByNameLookup = TypeByNameLookup.empty
            , typeInference = TypeInference.fromProjectToModule projectContext
            , variableRecord = Dict.empty
            , variableRegister = Variable.emptyRegister
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


moduleDefinitionVisitor : Node Module -> ModuleContext -> ModuleContext
moduleDefinitionVisitor moduleNode moduleContext =
    case Module.exposingList (Node.value moduleNode) of
        Exposing.All _ ->
            { moduleContext | exposes = ExposesEverything }

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
            { moduleContext | exposes = ExposesExplicitly (Set.fromList names) }



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
                    List.filterMap (registerDeclaration exposedNames) nodes
                        |> Debug.log "01. declarationListVisitor recordRegister"
            in
            ( [], { context | recordRegister = Variable.addVariables variables context.recordRegister } )


registerDeclaration : Set String -> Node Declaration -> Maybe ( String, Variable )
registerDeclaration exposedNames node =
    case Node.value node of
        Declaration.AliasDeclaration typeAlias ->
            case Node.value typeAlias.typeAnnotation of
                TypeAnnotation.Record recordDefinition ->
                    let
                        name : String
                        name =
                            Node.value typeAlias.name

                        recordFields : List (Node String)
                        recordFields =
                            List.map (Node.value >> Tuple.first) recordDefinition
                    in
                    if not (Set.member name exposedNames) && List.isEmpty typeAlias.generics then
                        Just ( name, Variable.newVariable recordFields Set.empty )

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing



-- DECLARATION ENTER VISITOR


declarationEnterVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationEnterVisitor node moduleContext =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            handleDeclaration { moduleContext | directAccessesToIgnore = RangeSet.empty } function

        Declaration.AliasDeclaration typeAlias ->
            case Node.value typeAlias.typeAnnotation of
                TypeAnnotation.Record recordDefinition ->
                    ( []
                    , List.foldl
                        (\(Node _ recordField) acc ->
                            case recordField of
                                ( _, Node _ (TypeAnnotation.Typed (Node _ ( [], name )) _) ) ->
                                    { acc
                                        | recordRegister =
                                            Variable.updateVariable name
                                                Variable.markAsUsedInAnUnknownManner
                                                acc.recordRegister
                                    }

                                _ ->
                                    acc
                        )
                        moduleContext
                        recordDefinition
                    )

                _ ->
                    ( [], moduleContext )

        _ ->
            ( [], moduleContext )


handleDeclaration : ModuleContext -> Expression.Function -> ( List (Error {}), ModuleContext )
handleDeclaration moduleContext { signature, declaration } =
    case Maybe.map (Node.value >> .typeAnnotation) signature of
        Just typeAnnotation ->
            let
                recordArguments : List (Maybe ( String, List (Node String) ))
                recordArguments =
                    recordDefinitionsFromTypeAnnotation moduleContext typeAnnotation
                        |> Debug.log "02. handleDeclaration recordArguments"

                arguments : List (Node Pattern)
                arguments =
                    (Node.value declaration).arguments
                        |> Debug.log "03. handleDeclaration arguments"

                variables : List ( String, Variable )
                variables =
                    List.map2
                        (\recordArgument argument ->
                            case recordArgument of
                                Just ( _, (_ :: _) as declaredFields ) ->
                                    createVariable declaredFields argument

                                _ ->
                                    Nothing
                        )
                        recordArguments
                        arguments
                        |> List.filterMap identity
                        |> Debug.log "04. handleDeclaration variables"
            in
            ( []
            , { moduleContext
                | variableRecord =
                    List.map2
                        (\recordArgument argument ->
                            case recordArgument of
                                Just ( recordName, (_ :: _) as declaredFields ) ->
                                    createVariable declaredFields argument
                                        |> Maybe.map (Tuple.mapSecond (\_ -> recordName))

                                _ ->
                                    Nothing
                        )
                        recordArguments
                        arguments
                        |> List.filterMap identity
                        |> Dict.fromList
                        |> Debug.log "04. handleDeclaration variableRecord"
                , variableRegister = Variable.addVariables variables moduleContext.variableRegister
              }
            )

        _ ->
            ( [], moduleContext )


recordDefinitionsFromTypeAnnotation : ModuleContext -> Node TypeAnnotation -> List (Maybe ( String, List (Node String) ))
recordDefinitionsFromTypeAnnotation moduleContext typeAnnotation =
    case Node.value typeAnnotation of
        TypeAnnotation.FunctionTypeAnnotation input output ->
            extractRecordDefinition moduleContext input :: recordDefinitionsFromTypeAnnotation moduleContext output

        _ ->
            []


extractRecordDefinition : ModuleContext -> Node TypeAnnotation -> Maybe ( String, List (Node String) )
extractRecordDefinition moduleContext typeAnnotation =
    case Node.value typeAnnotation of
        TypeAnnotation.Typed moduleNameAndName args ->
            case ( Node.value moduleNameAndName, args ) of
                ( ( [], name ), [] ) ->
                    moduleContext.recordRegister
                        |> Variable.getVariableByName name
                        |> Maybe.map (\variable -> ( name, Variable.declaredFields variable ))

                _ ->
                    Nothing

        _ ->
            Nothing


createVariable : List (Node String) -> Node Pattern -> Maybe ( String, Variable )
createVariable declaredFields argument =
    case Node.value argument of
        Pattern.VarPattern name ->
            Just ( name, Variable.newVariable declaredFields Set.empty )

        Pattern.ParenthesizedPattern pattern ->
            createVariable declaredFields pattern

        Pattern.AsPattern pattern name ->
            Just ( Node.value name, Variable.newVariable declaredFields (Set.fromList (fieldsFromPattern pattern)) )

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


type VariableOrError
    = VariableOrError_Variable ( String, Variable )
    | VariableOrError_Errors (List (Node String))


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


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    case Debug.log "05. expressionEnterVisitor" (Node.value node) of
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
                            (input :: argumentsAndReturnType output)
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

        Expression.OperatorApplication _ _ _ _ ->
            ( [], context )

        Expression.FunctionOrValue [] name ->
            if RangeSet.member (Node.range node) context.directAccessesToIgnore then
                ( [], updateRegister name Variable.markAsUsed context )

            else
                ( [], updateRegister name Variable.markAsUsedInAnUnknownManner context )

        Expression.FunctionOrValue _ _ ->
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

        Expression.RecordExpr _ ->
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

        Expression.Floatable _ ->
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

        Type.Function _ _ ->
            False

        Type.Tuple _ ->
            False

        Type.Type _ _ types ->
            List.any (isGenericUsed genericToFind) types

        Type.Record _ ->
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
                |> List.map (\(Node _ ( declaredField, _ )) -> declaredField)
                |> List.filter (\declaredField -> not (Set.member (Node.value declaredField) usedFields))
                |> ArgumentMatch_ReportErrors
                |> Just

        Expression.ParenthesizedExpression expr ->
            matchRecordWithArgument fields expr

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



-- EXPRESSION EXIT VISITOR


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionExitVisitor node context =
    case Debug.log "06. expressionExitVisitor" (Node.value node) of
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



-- DECLARATION EXIT VISITOR


declarationExitVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationExitVisitor node moduleContext =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            let
                _ =
                    Debug.log "07. declarationExitVisitor" moduleContext.variableRegister

                updatedRecordRegister =
                    Dict.foldl
                        (\variableName recordName recordRegister ->
                            case Variable.getVariableByName variableName moduleContext.variableRegister of
                                Just variable ->
                                    Variable.updateVariable recordName
                                        (\recordVariable ->
                                            let
                                                usedFields =
                                                    Set.toList (Variable.usedFields variable)

                                                updateWasUsed =
                                                    if Variable.wasUsed variable then
                                                        Variable.markAsUsed

                                                    else
                                                        identity

                                                updateWasUsedInAnUnknownManner =
                                                    if Variable.wasUsedInAnUnknownManner variable then
                                                        Variable.markAsUsedInAnUnknownManner

                                                    else
                                                        identity
                                            in
                                            recordVariable
                                                |> Variable.markFieldsAsUsed usedFields
                                                |> updateWasUsed
                                                |> updateWasUsedInAnUnknownManner
                                        )
                                        recordRegister

                                Nothing ->
                                    recordRegister
                        )
                        moduleContext.recordRegister
                        moduleContext.variableRecord
            in
            ( []
            , { moduleContext
                | recordRegister = updatedRecordRegister
                , variableRecord = Dict.empty
                , variableRegister = Variable.emptyRegister
              }
            )

        _ ->
            ( [], moduleContext )



-- FINAL EVALUATION


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation context =
    context.recordRegister
        |> Debug.log "08. finalEvaluation recordRegister"
        |> Variable.unusedDeclaredFieldsForScope
        |> Tuple.first
        |> List.map createError


createError : Node String -> Error {}
createError (Node range name) =
    Rule.error
        { message = "Unused field `" ++ name ++ "`"
        , details = [ "This field has been declared and may have been assigned to, but is never used. You may have forgotten to use it where needed. Please do so or remove the field." ]
        }
        range
