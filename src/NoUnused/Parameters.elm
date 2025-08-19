module NoUnused.Parameters exposing (rule)

{-| Report parameters that are not used.

@docs rule

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import List.Extra
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import NoUnused.Parameters.ParameterPath as ParameterPath exposing (Nesting(..), Path)
import Review.Fix as Fix exposing (Edit, Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (FixV2, ModuleKey, Rule)
import Set exposing (Set)


{-| Report parameters that are not used.

🔧 Running with `--fix` will automatically remove some of the reported errors.

    config =
        [ NoUnused.Parameters.rule
        ]

This rule looks within function arguments, let functions and lambdas to find any values that are unused. It will report any parameters that are not used.


## Fixes for lambdas

We're only offering fixes for lambdas here because we believe unused parameters in functions are a code smell that should be refactored.


## Fail

Value `number` is not used:

    add1 number =
        1

The rule will also report parameters that are only used to be passed again to the containing recursive function:

    last list unused =
        case list of
            [] ->
                Nothing

            [ a ] ->
                Just a

            _ :: rest ->
                last rest unused


## Success

    add1 number =
        number + 1


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Parameters
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.Parameters" initialContext
        |> Rule.withDirectDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextWithErrors
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    ( []
    , { dependencyModules =
            Dict.foldr
                (\_ dependency set ->
                    List.foldl
                        (\{ name } subSet ->
                            Set.insert (String.split "." name) subSet
                        )
                        set
                        (Dependency.modules dependency)
                )
                Set.empty
                dependencies
      , toReport = projectContext.toReport
      , functionCallsWithArguments = projectContext.functionCallsWithArguments
      }
    )


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionEnterVisitor node context ))
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withLetDeclarationEnterVisitor letDeclarationEnterVisitor
        |> Rule.withLetDeclarationExitVisitor letDeclarationExitVisitor
        |> Rule.providesFixesForModuleRule



--- CONTEXT


type alias ProjectContext =
    { dependencyModules : Set ModuleName
    , toReport : Dict ModuleName { key : ModuleKey, args : List ArgumentToReport }
    , functionCallsWithArguments : Dict ( ModuleName, FunctionName ) (List { key : ModuleKey, callSites : List CallSite })
    }


type alias CallSite =
    { fnNameEnd : Location
    , arguments : Array Range
    }


initialContext : ProjectContext
initialContext =
    { dependencyModules = Set.empty
    , toReport = Dict.empty
    , functionCallsWithArguments = Dict.empty
    }


type alias ModuleContext =
    { dependencyModules : Set ModuleName
    , lookupTable : ModuleNameLookupTable
    , scopes : Nonempty Scope
    , recursiveFunctions : Dict String FunctionArgs
    , locationsToIgnoreForRecursiveArguments : LocationsToIgnore
    , functionCallsWithArguments : Dict FunctionName (List CallSite)
    , functionCallsWithArgumentsForOtherModules : Dict ( ModuleName, FunctionName ) (List CallSite)
    , locationsToIgnoreFunctionCalls : List Location
    , functionsFromOtherModulesToFix : Set ( ModuleName, FunctionName )
    }


type alias Scope =
    { functionName : FunctionName
    , declared : List Declared
    , used : Set String
    , usedRecursively : Set String
    , toReport : List ArgumentToReport
    , locationsToIgnoreForFunctionCalls : List Location
    }


type alias FunctionName =
    String


type alias ArgumentToReport =
    { functionName : FunctionName
    , position : Int
    , nesting : Array Nesting
    , rangesToRemove : List Range
    , details : { message : String, details : List String }
    , range : Range
    , backupWhenFixImpossible : BackupWhenFixImpossible
    }


type BackupWhenFixImpossible
    = FixWith (() -> List Edit)
    | DontReportError


type alias Declared =
    { name : String
    , range : Range
    , kind : Kind
    , source : Source
    , position : Int
    , nesting : Array Nesting
    , rangesToRemove : Maybe (List Range)
    , toIgnoredFix : List Fix
    , backupWhenFixImpossible : BackupWhenFixImpossible
    }


type alias LocationsToIgnore =
    Dict String (List Range)


type alias FunctionArgs =
    Dict Int String


type Kind
    = Parameter
    | Alias
    | TupleWithoutVariables


type Source
    = NamedFunction
    | Lambda


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContent ->
            { dependencyModules = projectContent.dependencyModules
            , lookupTable = lookupTable
            , scopes =
                NonemptyList.fromElement
                    { functionName = "root"
                    , declared = []
                    , used = Set.empty
                    , usedRecursively = Set.empty
                    , toReport = []
                    , locationsToIgnoreForFunctionCalls = []
                    }
            , recursiveFunctions = Dict.empty
            , locationsToIgnoreForRecursiveArguments = Dict.empty
            , functionCallsWithArguments = Dict.empty
            , functionCallsWithArgumentsForOtherModules = Dict.empty
            , locationsToIgnoreFunctionCalls = []
            , functionsFromOtherModulesToFix =
                Dict.foldl
                    (\moduleName { args } set ->
                        List.foldl (\{ functionName } setAcc -> Set.insert ( moduleName, functionName ) setAcc) set args
                    )
                    Set.empty
                    projectContent.toReport
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ( List (Rule.Error {}), ProjectContext )
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName moduleKey ast moduleContext ->
            let
                isExposed : String -> Bool
                isExposed =
                    case Module.exposingList (Node.value ast.moduleDefinition) of
                        Exposing.All _ ->
                            always True

                        Exposing.Explicit explicitlyExposed ->
                            let
                                exposed : Set String
                                exposed =
                                    collectExposedElements explicitlyExposed
                            in
                            \name -> Set.member name exposed

                { errors, toReport, functionCallsWithArguments } =
                    List.foldl
                        (\arg acc ->
                            if isExposed arg.functionName then
                                let
                                    key : ( ModuleName, FunctionName )
                                    key =
                                        ( moduleName, arg.functionName )
                                in
                                { errors = acc.errors
                                , toReport = arg :: acc.toReport
                                , functionCallsWithArguments =
                                    case Dict.get arg.functionName moduleContext.functionCallsWithArguments of
                                        Just callSites ->
                                            case Dict.get key acc.functionCallsWithArguments of
                                                Just previous ->
                                                    Dict.insert key ({ key = moduleKey, callSites = callSites } :: previous) acc.functionCallsWithArguments

                                                Nothing ->
                                                    Dict.insert key [ { key = moduleKey, callSites = callSites } ] acc.functionCallsWithArguments

                                        Nothing ->
                                            acc.functionCallsWithArguments
                                }

                            else
                                { errors = List.Extra.maybeCons (reportError moduleContext.functionCallsWithArguments arg) acc.errors
                                , toReport = acc.toReport
                                , functionCallsWithArguments = acc.functionCallsWithArguments
                                }
                        )
                        { errors = []
                        , toReport = []
                        , functionCallsWithArguments = Dict.map (\_ callSites -> [ { key = moduleKey, callSites = callSites } ]) moduleContext.functionCallsWithArgumentsForOtherModules
                        }
                        (NonemptyList.head moduleContext.scopes).toReport
            in
            ( errors
            , { dependencyModules = Set.empty
              , toReport = Dict.singleton moduleName { key = moduleKey, args = toReport }
              , functionCallsWithArguments = functionCallsWithArguments
              }
            )
        )
        |> Rule.withModuleName
        |> Rule.withModuleKey
        |> Rule.withFullAst


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { dependencyModules = previousContext.dependencyModules
    , toReport = Dict.union newContext.toReport previousContext.toReport
    , functionCallsWithArguments = mergeFunctionCallsWithArguments previousContext.functionCallsWithArguments newContext.functionCallsWithArguments
    }


mergeFunctionCallsWithArguments :
    Dict ( ModuleName, FunctionName ) (List { key : ModuleKey, callSites : List CallSite })
    -> Dict ( ModuleName, FunctionName ) (List { key : ModuleKey, callSites : List CallSite })
    -> Dict ( ModuleName, FunctionName ) (List { key : ModuleKey, callSites : List CallSite })
mergeFunctionCallsWithArguments new previous =
    Dict.foldl
        (\key newDict acc ->
            case Dict.get key acc of
                Just previousList ->
                    Dict.insert key (newDict ++ previousList) acc

                Nothing ->
                    Dict.insert key newDict acc
        )
        previous
        new


finalEvaluation : ProjectContext -> List (Rule.Error scope)
finalEvaluation projectContext =
    Dict.foldl
        (\moduleName { key, args } acc ->
            List.filterMap
                (\arg ->
                    let
                        toError : List FixV2 -> Maybe (Rule.Error scope)
                        toError fixes =
                            Rule.errorForModule key arg.details arg.range
                                |> Rule.withFixesV2 fixes
                                |> Just
                    in
                    case
                        Dict.get ( moduleName, arg.functionName ) projectContext.functionCallsWithArguments
                            |> Maybe.withDefault []
                            |> (\callSitesPerFile -> applyFixesAcrossModules arg callSitesPerFile [ Rule.editModule key (List.map Fix.removeRange arg.rangesToRemove) ])
                    of
                        Just edits ->
                            toError edits

                        Nothing ->
                            case arg.backupWhenFixImpossible of
                                FixWith backupEdits ->
                                    toError [ Rule.editModule key (backupEdits ()) ]

                                DontReportError ->
                                    Nothing
                )
                args
                ++ acc
        )
        []
        projectContext.toReport


applyFixesAcrossModules : ArgumentToReport -> List { key : ModuleKey, callSites : List CallSite } -> List FixV2 -> Maybe (List FixV2)
applyFixesAcrossModules arg callSitesPerFile fixesSoFar =
    case callSitesPerFile of
        [] ->
            Just fixesSoFar

        { key, callSites } :: rest ->
            case addArgumentToRemove arg.position callSites [] of
                Nothing ->
                    Nothing

                Just rangesToRemove ->
                    applyFixesAcrossModules
                        arg
                        rest
                        (Rule.editModule key (List.map Fix.removeRange rangesToRemove) :: fixesSoFar)



-- MODULE DEFINITION


collectExposedElements : List (Node Exposing.TopLevelExpose) -> Set String
collectExposedElements exposed =
    List.foldl
        (\exp set ->
            case Node.value exp of
                Exposing.FunctionExpose name ->
                    Set.insert name set

                _ ->
                    set
        )
        Set.empty
        exposed



-- DECLARATION VISITOR


declarationEnterVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration f ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value f.declaration

                declared : List (List Declared)
                declared =
                    findDeclared NamedFunction (Node.range declaration.name).end declaration.arguments f.signature

                functionName : FunctionName
                functionName =
                    Node.value declaration.name
            in
            ( []
            , { dependencyModules = context.dependencyModules
              , lookupTable = context.lookupTable
              , scopes =
                    NonemptyList.cons
                        { functionName = functionName
                        , declared = List.concat declared
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = []
                        , locationsToIgnoreForFunctionCalls = []
                        }
                        context.scopes
              , recursiveFunctions = Dict.singleton functionName (getArgNames declared)
              , locationsToIgnoreForRecursiveArguments = Dict.empty
              , functionCallsWithArguments = context.functionCallsWithArguments
              , functionCallsWithArgumentsForOtherModules = context.functionCallsWithArgumentsForOtherModules
              , functionsFromOtherModulesToFix = context.functionsFromOtherModulesToFix
              , locationsToIgnoreFunctionCalls = []
              }
            )

        _ ->
            ( [], context )


declarationExitVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationExitVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            report context

        _ ->
            ( [], context )


getArgNames : List (List Declared) -> FunctionArgs
getArgNames declared =
    getArgNamesHelp declared 0 Dict.empty


getArgNamesHelp : List (List Declared) -> Int -> FunctionArgs -> FunctionArgs
getArgNamesHelp declared index acc =
    case declared of
        [] ->
            acc

        args :: restOfDeclared ->
            let
                newAcc : Dict Int String
                newAcc =
                    case args of
                        [ arg ] ->
                            Dict.insert index arg.name acc

                        _ ->
                            acc
            in
            getArgNamesHelp restOfDeclared (index + 1) newAcc


getParametersFromPatterns : ParameterPath.Path -> Location -> Source -> Node Pattern -> List Declared
getParametersFromPatterns path previousEnd source node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            getParametersFromPatterns path previousEnd source pattern

        Pattern.VarPattern name ->
            [ { name = name
              , range = Node.range node
              , kind = Parameter
              , rangesToRemove =
                    if Array.isEmpty path.nesting then
                        ParameterPath.fix path [ { start = previousEnd, end = (Node.range node).end } ]

                    else
                        Nothing
              , toIgnoredFix = [ Fix.replaceRangeBy (Node.range node) "_" ]
              , position = path.index
              , nesting = path.nesting
              , source = source
              , backupWhenFixImpossible = FixWith (always [])
              }
            ]

        Pattern.AllPattern ->
            if Array.isEmpty path.nesting && source == NamedFunction then
                case ParameterPath.fix path [ { start = previousEnd, end = (Node.range node).end } ] of
                    Just rangesToRemove ->
                        [ { name = "_"
                          , range = Node.range node
                          , kind = Parameter
                          , rangesToRemove = Just rangesToRemove
                          , toIgnoredFix = []
                          , position = path.index
                          , nesting = path.nesting
                          , source = source
                          , backupWhenFixImpossible = DontReportError
                          }
                        ]

                    Nothing ->
                        []

            else
                []

        Pattern.AsPattern pattern asName ->
            getParametersFromAsPattern path previousEnd source pattern asName

        Pattern.RecordPattern fields ->
            let
                pathInArgument_ : Path
                pathInArgument_ =
                    ParameterPath.inRecord path
            in
            case fields of
                [ field ] ->
                    [ { name = Node.value field
                      , range = Node.range field
                      , kind = Parameter
                      , rangesToRemove = Nothing
                      , toIgnoredFix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                      , position = path.index
                      , nesting = pathInArgument_.nesting
                      , source = source
                      , backupWhenFixImpossible = FixWith (always [])
                      }
                    ]

                _ ->
                    let
                        fieldNames : List String
                        fieldNames =
                            List.map Node.value fields
                    in
                    List.map
                        (\field ->
                            { name = Node.value field
                            , range = Node.range field
                            , kind = Parameter
                            , rangesToRemove = Nothing
                            , toIgnoredFix =
                                [ Fix.replaceRangeBy
                                    (Node.range node)
                                    (fieldNames |> List.filter (\f -> f /= Node.value field) |> formatRecord)
                                ]
                            , position = path.index
                            , nesting = pathInArgument_.nesting
                            , source = source
                            , backupWhenFixImpossible = FixWith (always [])
                            }
                        )
                        fields

        Pattern.TuplePattern patterns ->
            let
                parametersFromPatterns : List Declared
                parametersFromPatterns =
                    patterns
                        |> List.indexedMap
                            (\tupleIndex pattern ->
                                getParametersFromPatterns
                                    (ParameterPath.inTuple tupleIndex path)
                                    -- TODO Use end from previous tuple element?
                                    previousEnd
                                    source
                                    pattern
                            )
                        |> List.concat
            in
            if List.isEmpty parametersFromPatterns && List.all isPatternWildCard patterns then
                [ { name = ""
                  , range = Node.range node
                  , kind = TupleWithoutVariables
                  , rangesToRemove = Nothing
                  , toIgnoredFix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                  , position = path.index
                  , nesting = path.nesting
                  , source = source
                  , backupWhenFixImpossible = FixWith (always [])
                  }
                ]

            else
                parametersFromPatterns

        Pattern.NamedPattern _ patterns ->
            patterns
                |> List.indexedMap
                    (\patternIndex pattern ->
                        getParametersFromPatterns
                            (ParameterPath.inNamedPattern patternIndex path)
                            -- TODO Use something else?
                            previousEnd
                            source
                            pattern
                    )
                |> List.concat

        _ ->
            []


getParametersFromAsPattern : Path -> Location -> Source -> Node Pattern -> Node String -> List Declared
getParametersFromAsPattern path previousEnd source pattern asName =
    let
        parametersFromPatterns : List Declared
        parametersFromPatterns =
            getParametersFromPatterns (ParameterPath.inAlias path) previousEnd source pattern

        asParameter : Declared
        asParameter =
            { name = Node.value asName
            , range = Node.range asName
            , kind = Alias
            , rangesToRemove = Nothing
            , toIgnoredFix = [ Fix.removeRange { start = (Node.range pattern).end, end = (Node.range asName).end } ]
            , position = path.index
            , nesting = path.nesting
            , source = source
            , backupWhenFixImpossible = FixWith (always [])
            }
    in
    asParameter :: parametersFromPatterns


isPatternWildCard : Node Pattern -> Bool
isPatternWildCard node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            isPatternWildCard pattern

        Pattern.AllPattern ->
            True

        _ ->
            False


formatRecord : List String -> String
formatRecord fields =
    "{ " ++ String.join ", " fields ++ " }"



-- EXPRESSION ENTER VISITOR


expressionEnterVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionEnterVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            markValueAsUsed (Node.range node) name context

        Expression.RecordUpdateExpression name _ ->
            markValueAsUsed (Node.range name) (Node.value name) context

        Expression.LambdaExpression { args } ->
            let
                start : Location
                start =
                    (Node.range node).start
            in
            { context
                | scopes =
                    NonemptyList.cons
                        { functionName = "dummy lambda"
                        , declared = findDeclared Lambda { row = start.row, column = start.column + 1 } args Nothing |> List.concat
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = []
                        , locationsToIgnoreForFunctionCalls = []
                        }
                        context.scopes
            }

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: arguments) ->
            registerFunctionCall fnName fnRange (List.map Node.range arguments) context

        Expression.OperatorApplication "|>" _ (Node { start } _) (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: arguments))) ->
            registerFunctionCall
                fnName
                fnRange
                (List.map Node.range arguments ++ [ { start = start, end = applicationRange.start } ])
                context

        Expression.OperatorApplication "<|" _ (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: arguments))) (Node { end } _) ->
            registerFunctionCall
                fnName
                fnRange
                (List.map Node.range arguments ++ [ { start = applicationRange.end, end = end } ])
                context

        _ ->
            context



-- EXPRESSION EXIT VISITOR


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionExitVisitor (Node _ node) context =
    case node of
        Expression.LambdaExpression _ ->
            report context

        _ ->
            ( [], context )


letDeclarationEnterVisitor : a -> Node Expression.LetDeclaration -> ModuleContext -> ( List nothing, ModuleContext )
letDeclarationEnterVisitor _ letDeclaration context =
    case Node.value letDeclaration of
        Expression.LetFunction function ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration
            in
            if List.isEmpty declaration.arguments then
                ( [], context )

            else
                let
                    functionName : FunctionName
                    functionName =
                        Node.value declaration.name

                    declared : List (List Declared)
                    declared =
                        findDeclared NamedFunction (Node.range declaration.name).end declaration.arguments function.signature

                    newScope : Scope
                    newScope =
                        { functionName = functionName
                        , declared = List.concat declared
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = []
                        , locationsToIgnoreForFunctionCalls = []
                        }
                in
                ( []
                , { context
                    | scopes = NonemptyList.cons newScope context.scopes
                    , recursiveFunctions = Dict.insert functionName (getArgNames declared) context.recursiveFunctions
                  }
                )

        Expression.LetDestructuring _ _ ->
            ( [], context )


letDeclarationExitVisitor : a -> Node Expression.LetDeclaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
letDeclarationExitVisitor _ letDeclaration context =
    case Node.value letDeclaration of
        Expression.LetFunction function ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration
            in
            if List.isEmpty declaration.arguments then
                ( [], context )

            else
                report context

        Expression.LetDestructuring _ _ ->
            ( [], context )


registerFunctionCall : FunctionName -> Range -> List Range -> ModuleContext -> ModuleContext
registerFunctionCall fnName fnRange arguments context =
    if isVariableOrFunctionName fnName && not (List.member fnRange.start context.locationsToIgnoreFunctionCalls) then
        case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
            Just [] ->
                case Dict.get fnName context.recursiveFunctions of
                    Just fnArgs ->
                        let
                            locationsToIgnore : LocationsToIgnore
                            locationsToIgnore =
                                ignoreLocationsForRecursiveArguments fnArgs arguments 0 context.locationsToIgnoreForRecursiveArguments
                        in
                        { context
                            | locationsToIgnoreForRecursiveArguments = locationsToIgnore
                            , locationsToIgnoreFunctionCalls = fnRange.start :: context.locationsToIgnoreFunctionCalls
                        }
                            |> markFunctionCall fnName fnRange.end (Array.fromList arguments)

                    Nothing ->
                        { context | locationsToIgnoreFunctionCalls = fnRange.start :: context.locationsToIgnoreFunctionCalls }
                            |> markFunctionCall fnName fnRange.end (Array.fromList arguments)

            Just moduleName ->
                let
                    key : ( ModuleName, FunctionName )
                    key =
                        ( moduleName, fnName )
                in
                if Set.member key context.functionsFromOtherModulesToFix then
                    -- TODO Do the same in markValueAsUsed
                    let
                        functionCallsWithArgumentsForOtherModules : Dict ( ModuleName, FunctionName ) (List CallSite)
                        functionCallsWithArgumentsForOtherModules =
                            case Dict.get key context.functionCallsWithArgumentsForOtherModules of
                                Just previous ->
                                    Dict.insert key ({ fnNameEnd = fnRange.end, arguments = Array.fromList arguments } :: previous) context.functionCallsWithArgumentsForOtherModules

                                Nothing ->
                                    Dict.insert key [ { fnNameEnd = fnRange.end, arguments = Array.fromList arguments } ] context.functionCallsWithArgumentsForOtherModules
                    in
                    { context
                        | locationsToIgnoreFunctionCalls = fnRange.start :: context.locationsToIgnoreFunctionCalls
                        , functionCallsWithArgumentsForOtherModules = functionCallsWithArgumentsForOtherModules
                    }

                else
                    { context | locationsToIgnoreFunctionCalls = fnRange.start :: context.locationsToIgnoreFunctionCalls }

            Nothing ->
                context

    else
        context


markFunctionCall : FunctionName -> Location -> Array Range -> ModuleContext -> ModuleContext
markFunctionCall fnName fnNameEnd arguments context =
    { context
        | functionCallsWithArguments =
            case Dict.get fnName context.functionCallsWithArguments of
                Just previous ->
                    Dict.insert fnName ({ fnNameEnd = fnNameEnd, arguments = arguments } :: previous) context.functionCallsWithArguments

                Nothing ->
                    Dict.insert fnName [ { fnNameEnd = fnNameEnd, arguments = arguments } ] context.functionCallsWithArguments
    }


ignoreLocationsForRecursiveArguments : FunctionArgs -> List Range -> Int -> LocationsToIgnore -> LocationsToIgnore
ignoreLocationsForRecursiveArguments fnArgs nodes index acc =
    case nodes of
        [] ->
            acc

        range :: rest ->
            let
                newAcc : LocationsToIgnore
                newAcc =
                    case Dict.get index fnArgs of
                        Just argName ->
                            insertInDictList argName range acc

                        Nothing ->
                            acc
            in
            ignoreLocationsForRecursiveArguments fnArgs rest (index + 1) newAcc


markValueAsUsed : Range -> String -> ModuleContext -> ModuleContext
markValueAsUsed range name context =
    if isVariableOrFunctionName name then
        { context
            | scopes =
                NonemptyList.mapHead
                    (\scope ->
                        -- TODO Avoid changing context if name is not a parameter
                        if shouldBeIgnored range name context then
                            { scope | usedRecursively = Set.insert name scope.usedRecursively }

                        else
                            { scope | used = Set.insert name scope.used }
                    )
                    context.scopes
            , functionCallsWithArguments =
                if List.member range.start context.locationsToIgnoreFunctionCalls then
                    context.functionCallsWithArguments

                else
                    Dict.insert name [ { fnNameEnd = range.end, arguments = Array.empty } ] context.functionCallsWithArguments
        }

    else
        context


isVariableOrFunctionName : String -> Bool
isVariableOrFunctionName name =
    case name |> String.slice 0 1 |> String.toList |> List.head of
        Just firstChar ->
            Char.isLower firstChar

        Nothing ->
            False


shouldBeIgnored : Range -> String -> ModuleContext -> Bool
shouldBeIgnored range name context =
    case Dict.get name context.locationsToIgnoreForRecursiveArguments of
        Just ranges ->
            List.any (isRangeIncluded range) ranges

        Nothing ->
            False


isRangeIncluded : Range -> Range -> Bool
isRangeIncluded inner outer =
    (Range.compareLocations inner.start outer.start /= LT)
        && (Range.compareLocations inner.end outer.end /= GT)


markAllAsUsed : Set String -> List ArgumentToReport -> Nonempty Scope -> Nonempty Scope
markAllAsUsed names toReport scopes =
    NonemptyList.mapHead
        (\scope ->
            { scope
                | used = Set.union names scope.used
                , toReport = toReport ++ scope.toReport
            }
        )
        scopes


report : ModuleContext -> ( List (Rule.Error {}), ModuleContext )
report context =
    let
        ( headScope, scopes ) =
            NonemptyList.headAndPop context.scopes

        { reportLater, reportNow, remainingUsed } =
            List.foldl
                (findErrorsAndVariablesNotPartOfScope headScope)
                { reportLater = [], reportNow = [], remainingUsed = headScope.used }
                headScope.declared

        ( errors, newContext ) =
            reportErrors headScope context reportNow
    in
    ( errors
    , { newContext
        | scopes = markAllAsUsed remainingUsed reportLater scopes
        , recursiveFunctions = Dict.remove headScope.functionName context.recursiveFunctions
      }
    )


reportErrors : Scope -> ModuleContext -> List (Rule.Error {}) -> ( List (Rule.Error {}), ModuleContext )
reportErrors scope context initialErrors =
    let
        ( errors, newFunctionCallsWithArguments ) =
            List.foldl
                (\arg ( errorAcc, functionCallsWithArguments ) ->
                    ( List.Extra.maybeCons (reportError context.functionCallsWithArguments arg) errorAcc
                    , Dict.remove arg.functionName functionCallsWithArguments
                    )
                )
                ( initialErrors, context.functionCallsWithArguments )
                scope.toReport
    in
    ( errors, { context | functionCallsWithArguments = newFunctionCallsWithArguments } )


reportError : Dict FunctionName (List CallSite) -> ArgumentToReport -> Maybe (Rule.Error {})
reportError functionCallsWithArguments { functionName, position, details, range, rangesToRemove, backupWhenFixImpossible } =
    case
        Dict.get functionName functionCallsWithArguments
            |> Maybe.andThen (\callArgumentList -> addArgumentToRemove position callArgumentList rangesToRemove)
    of
        Just edits ->
            edits
                |> List.map Fix.removeRange
                |> Rule.errorWithFix details range
                |> Just

        Nothing ->
            case backupWhenFixImpossible of
                FixWith edits ->
                    Rule.errorWithFix details range (edits ())
                        |> Just

                DontReportError ->
                    Nothing


addArgumentToRemove : Int -> List CallSite -> List Range -> Maybe (List Range)
addArgumentToRemove position callSites acc =
    case callSites of
        [] ->
            Just acc

        callSite :: rest ->
            case Array.get position callSite.arguments of
                Just range ->
                    let
                        previousEnd : Location
                        previousEnd =
                            Array.get (position - 1) callSite.arguments
                                |> Maybe.map .end
                                |> Maybe.withDefault callSite.fnNameEnd
                    in
                    addArgumentToRemove position rest ({ start = previousEnd, end = range.end } :: acc)

                Nothing ->
                    -- If an argument at that location could not be found, then we can't autofix the issue.
                    Nothing


findErrorsAndVariablesNotPartOfScope :
    Scope
    -> Declared
    -> { reportLater : List ArgumentToReport, reportNow : List (Rule.Error {}), remainingUsed : Set String }
    -> { reportLater : List ArgumentToReport, reportNow : List (Rule.Error {}), remainingUsed : Set String }
findErrorsAndVariablesNotPartOfScope scope declared ({ reportLater, reportNow, remainingUsed } as acc) =
    if Set.member declared.name scope.usedRecursively then
        -- If variable was used as a recursive argument
        if Set.member declared.name remainingUsed then
            -- If variable was used somewhere else as well
            { reportLater = reportLater
            , reportNow = reportNow
            , remainingUsed = Set.remove declared.name remainingUsed
            }

        else
            -- If variable was used ONLY as a recursive argument
            recursiveParameterError scope.functionName declared
                |> accumulate acc

    else if Set.member declared.name remainingUsed then
        { reportLater = reportLater
        , reportNow = reportNow
        , remainingUsed = Set.remove declared.name remainingUsed
        }

    else
        errorsForValue scope.functionName declared
            |> accumulate acc


accumulate :
    { reportLater : List ArgumentToReport, reportNow : List (Rule.Error {}), remainingUsed : Set String }
    -> ReportTime
    -> { reportLater : List ArgumentToReport, reportNow : List (Rule.Error {}), remainingUsed : Set String }
accumulate { reportLater, reportNow, remainingUsed } reportTime =
    case reportTime of
        ReportNow error ->
            { reportLater = reportLater
            , reportNow = error :: reportNow
            , remainingUsed = remainingUsed
            }

        ReportLater error ->
            { reportLater = error :: reportLater
            , reportNow = reportNow
            , remainingUsed = remainingUsed
            }


insertInDictList : comparable -> value -> Dict comparable (List value) -> Dict comparable (List value)
insertInDictList key value dict =
    case Dict.get key dict of
        Nothing ->
            Dict.insert key [ value ] dict

        Just previous ->
            Dict.insert key (value :: previous) dict


findDeclared : Source -> Location -> List (Node Pattern) -> Maybe (Node Signature) -> List (List Declared)
findDeclared source previousEnd arguments signature =
    findDeclaredHelp
        source
        previousEnd
        0
        (ParameterPath.fromSignature signature)
        arguments
        []


findDeclaredHelp : Source -> Location -> Int -> Path -> List (Node Pattern) -> List (List Declared) -> List (List Declared)
findDeclaredHelp source previousEnd index path arguments acc =
    case arguments of
        [] ->
            List.reverse acc

        arg :: remainingArguments ->
            findDeclaredHelp
                source
                (Node.range arg).end
                (index + 1)
                (ParameterPath.nextArgument path)
                remainingArguments
                (getParametersFromPatterns path previousEnd source arg :: acc)


type ReportTime
    = ReportNow (Rule.Error {})
    | ReportLater ArgumentToReport


errorsForValue : FunctionName -> Declared -> ReportTime
errorsForValue functionName ({ name, kind, range, source, position, nesting, rangesToRemove, toIgnoredFix } as arg) =
    case kind of
        Parameter ->
            reportParameter
                { message = "Parameter `" ++ name ++ "` is not used"
                , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                }
                (always [])
                functionName
                arg

        Alias ->
            Rule.errorWithFix
                { message = "Pattern alias `" ++ name ++ "` is not used"
                , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                }
                range
                toIgnoredFix
                |> ReportNow

        TupleWithoutVariables ->
            Rule.errorWithFix
                { message = "Tuple pattern is not needed"
                , details = [ "You should remove this pattern." ]
                }
                range
                (case source of
                    NamedFunction ->
                        []

                    Lambda ->
                        toIgnoredFix
                )
                |> ReportNow


reportParameter : { message : String, details : List String } -> (() -> List Edit) -> FunctionName -> Declared -> ReportTime
reportParameter details backupFix functionName arg =
    case arg.source of
        NamedFunction ->
            case arg.rangesToRemove of
                Just rangesToRemove_ ->
                    ReportLater
                        { functionName = functionName
                        , position = arg.position
                        , nesting = arg.nesting
                        , details = details
                        , range = arg.range
                        , rangesToRemove = rangesToRemove_
                        , backupWhenFixImpossible = arg.backupWhenFixImpossible
                        }

                Nothing ->
                    ReportNow (Rule.errorWithFix details arg.range [])

        Lambda ->
            ReportNow (Rule.errorWithFix details arg.range arg.toIgnoredFix)


recursiveParameterError : String -> Declared -> ReportTime
recursiveParameterError functionName arg =
    let
        details : { message : String, details : List String }
        details =
            { message = "Parameter `" ++ arg.name ++ "` is only used in recursion"
            , details =
                [ "This parameter is only used to be passed as an argument to '" ++ functionName ++ "', but its value is never read or used."
                , "You should either use this parameter somewhere, or remove it at the location I pointed at."
                ]
            }
    in
    case arg.kind of
        Parameter ->
            reportParameter details (always []) functionName arg

        _ ->
            Rule.error details arg.range
                |> ReportNow
