module NoUnused.Parameters exposing (rule)

{-| Report parameters that are not used.

@docs rule

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import NoUnused.Parameters.ParameterPath as ParameterPath exposing (Nesting(..), Path)
import Review.Fix as Fix exposing (Edit, Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
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
    Rule.newModuleRuleSchemaUsingContextCreator "NoUnused.Parameters" initialContext
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionEnterVisitor node context ))
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withLetDeclarationEnterVisitor letDeclarationEnterVisitor
        |> Rule.withLetDeclarationExitVisitor letDeclarationExitVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema



--- CONTEXT


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , scopes : Nonempty Scope
    , recursiveFunctions : Dict String FunctionArgs
    , locationsToIgnoreForRecursiveArguments : LocationsToIgnore
    , functionCallsWithArguments : Dict FunctionName (List (Array Range))
    }


type alias Scope =
    { functionName : FunctionName
    , declared : List Declared
    , used : Set String
    , usedRecursively : Set String
    , toReport : ToReport
    , locationsToIgnoreForFunctionCalls : List Location
    }


type alias FunctionName =
    String


type alias ToReport =
    Dict FunctionName (Dict Int (List ArgumentToReport))


type alias ArgumentToReport =
    { functionName : FunctionName
    , position : Int
    , nesting : Array Nesting
    , rangesToRemove : List Range
    , toError : List Edit -> Rule.Error {}
    }


type alias Declared =
    { name : String
    , range : Range
    , kind : Kind
    , source : Source
    , position : Int
    , pathInArgument : Array Nesting
    , rangesToRemove : Maybe (List Range)
    , toIgnoredFix : List Fix
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


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , scopes =
                NonemptyList.fromElement
                    { functionName = "root"
                    , declared = []
                    , used = Set.empty
                    , usedRecursively = Set.empty
                    , toReport = Dict.empty
                    , locationsToIgnoreForFunctionCalls = []
                    }
            , recursiveFunctions = Dict.empty
            , locationsToIgnoreForRecursiveArguments = Dict.empty
            , functionCallsWithArguments = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable



-- DECLARATION VISITOR


declarationEnterVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration f ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value f.declaration

                declared : List (List Declared)
                declared =
                    findDeclared NamedFunction declaration.arguments f.signature

                functionName : FunctionName
                functionName =
                    Node.value declaration.name
            in
            ( []
            , { lookupTable = context.lookupTable
              , scopes =
                    NonemptyList.cons
                        { functionName = functionName
                        , declared = List.concat declared
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = Dict.empty
                        , locationsToIgnoreForFunctionCalls = []
                        }
                        context.scopes
              , recursiveFunctions = Dict.singleton functionName (getArgNames declared)
              , locationsToIgnoreForRecursiveArguments = Dict.empty
              , functionCallsWithArguments = context.functionCallsWithArguments
              }
            )

        _ ->
            ( [], context )


declarationExitVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
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


getParametersFromPatterns : ParameterPath.Path -> Source -> Node Pattern -> List Declared
getParametersFromPatterns path source node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            getParametersFromPatterns path source pattern

        Pattern.VarPattern name ->
            [ { name = name
              , range = Node.range node
              , kind = Parameter
              , rangesToRemove =
                    if Array.isEmpty path.nesting then
                        ParameterPath.fix path [ Node.range node ]

                    else
                        Nothing
              , toIgnoredFix = [ Fix.replaceRangeBy (Node.range node) "_" ]
              , position = path.index
              , pathInArgument = path.nesting
              , source = source
              }
            ]

        Pattern.AsPattern pattern asName ->
            getParametersFromAsPattern path source pattern asName

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
                      , pathInArgument = pathInArgument_.nesting
                      , source = source
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
                            , pathInArgument = pathInArgument_.nesting
                            , source = source
                            }
                        )
                        fields

        Pattern.TuplePattern patterns ->
            let
                parametersFromPatterns : List Declared
                parametersFromPatterns =
                    patterns
                        |> List.indexedMap (\tupleIndex pattern -> getParametersFromPatterns (ParameterPath.inTuple tupleIndex path) source pattern)
                        |> List.concat
            in
            if List.isEmpty parametersFromPatterns && List.all isPatternWildCard patterns then
                [ { name = ""
                  , range = Node.range node
                  , kind = TupleWithoutVariables
                  , rangesToRemove = Nothing
                  , toIgnoredFix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                  , position = path.index
                  , pathInArgument = path.nesting
                  , source = source
                  }
                ]

            else
                parametersFromPatterns

        Pattern.NamedPattern _ patterns ->
            patterns
                |> List.indexedMap (\patternIndex pattern -> getParametersFromPatterns (ParameterPath.inNamedPattern patternIndex path) source pattern)
                |> List.concat

        _ ->
            []


getParametersFromAsPattern : Path -> Source -> Node Pattern -> Node String -> List Declared
getParametersFromAsPattern path source pattern asName =
    let
        parametersFromPatterns : List Declared
        parametersFromPatterns =
            getParametersFromPatterns (ParameterPath.inAlias path) source pattern

        asParameter : Declared
        asParameter =
            { name = Node.value asName
            , range = Node.range asName
            , kind = Alias
            , rangesToRemove = Nothing
            , toIgnoredFix = [ Fix.removeRange { start = (Node.range pattern).end, end = (Node.range asName).end } ]
            , position = path.index
            , pathInArgument = path.nesting
            , source = source
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


expressionEnterVisitor : Node Expression -> Context -> Context
expressionEnterVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            markValueAsUsed (Node.range node) name context

        Expression.RecordUpdateExpression name _ ->
            markValueAsUsed (Node.range name) (Node.value name) context

        Expression.LambdaExpression { args } ->
            { context
                | scopes =
                    NonemptyList.cons
                        { functionName = "dummy lambda"
                        , declared = findDeclared Lambda args Nothing |> List.concat
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = Dict.empty
                        , locationsToIgnoreForFunctionCalls = []
                        }
                        context.scopes
            }

        Expression.Application ((Node fnRange (Expression.FunctionOrValue [] fnName)) :: arguments) ->
            registerFunctionCall fnName fnRange (List.map Node.range arguments) context

        Expression.OperatorApplication "|>" _ (Node { start } _) (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue [] fnName)) :: arguments))) ->
            registerFunctionCall
                fnName
                fnRange
                (List.map Node.range arguments ++ [ { start = start, end = applicationRange.start } ])
                context

        Expression.OperatorApplication "<|" _ (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue [] fnName)) :: arguments))) (Node { end } _) ->
            registerFunctionCall
                fnName
                fnRange
                (List.map Node.range arguments ++ [ { start = applicationRange.end, end = end } ])
                context

        _ ->
            context



-- EXPRESSION EXIT VISITOR


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor (Node _ node) context =
    case node of
        Expression.LambdaExpression _ ->
            report context

        _ ->
            ( [], context )


letDeclarationEnterVisitor : a -> Node Expression.LetDeclaration -> Context -> ( List nothing, Context )
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
                        findDeclared NamedFunction declaration.arguments function.signature

                    newScope : Scope
                    newScope =
                        { functionName = functionName
                        , declared = List.concat declared
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = Dict.empty
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


letDeclarationExitVisitor : a -> Node Expression.LetDeclaration -> Context -> ( List (Rule.Error {}), Context )
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


registerFunctionCall : FunctionName -> Range -> List Range -> Context -> Context
registerFunctionCall fnName fnRange arguments context =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
        Just [] ->
            case Dict.get fnName context.recursiveFunctions of
                Just fnArgs ->
                    let
                        locationsToIgnore : LocationsToIgnore
                        locationsToIgnore =
                            ignoreLocationsForRecursiveArguments fnArgs arguments 0 context.locationsToIgnoreForRecursiveArguments
                    in
                    { context | locationsToIgnoreForRecursiveArguments = locationsToIgnore }
                        |> markFunctionCall fnName (Array.fromList arguments)

                Nothing ->
                    context
                        |> markFunctionCall fnName (Array.fromList arguments)

        Just moduleName ->
            -- TODO Handle function calls from other modules
            context

        Nothing ->
            context


markFunctionCall : FunctionName -> Array Range -> Context -> Context
markFunctionCall fnName arguments context =
    { context
        | functionCallsWithArguments =
            case Dict.get fnName context.functionCallsWithArguments of
                Just previous ->
                    Dict.insert fnName (arguments :: previous) context.functionCallsWithArguments

                Nothing ->
                    Dict.insert fnName [ arguments ] context.functionCallsWithArguments
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


finalEvaluation : Context -> List (Rule.Error {})
finalEvaluation context =
    reportErrors context (NonemptyList.head context.scopes)


markValueAsUsed : Range -> String -> Context -> Context
markValueAsUsed range name context =
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
    }


shouldBeIgnored : Range -> String -> Context -> Bool
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


markAllAsUsed : Set String -> FunctionName -> Dict Int (List ArgumentToReport) -> Nonempty Scope -> Nonempty Scope
markAllAsUsed names functionName toReport scopes =
    NonemptyList.mapHead
        (\scope ->
            { scope
                | used = Set.union names scope.used
                , toReport =
                    if Dict.isEmpty toReport then
                        scope.toReport

                    else
                        Dict.insert functionName toReport scope.toReport
            }
        )
        scopes


report : Context -> ( List (Rule.Error {}), Context )
report context =
    let
        ( headScope, scopes ) =
            NonemptyList.headAndPop context.scopes

        { reportLater, reportNow, remainingUsed } =
            List.foldl
                (findErrorsAndVariablesNotPartOfScope headScope)
                { reportLater = Dict.empty, reportNow = [], remainingUsed = headScope.used }
                headScope.declared
    in
    ( reportErrors context headScope ++ reportNow
    , { context
        | scopes = markAllAsUsed remainingUsed headScope.functionName reportLater scopes
        , recursiveFunctions = Dict.remove headScope.functionName context.recursiveFunctions
      }
    )


reportErrors : Context -> Scope -> List (Rule.Error {})
reportErrors context scope =
    scope.toReport
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concat
        |> List.map (reportError context.functionCallsWithArguments)


reportError : Dict FunctionName (List (Array Range)) -> ArgumentToReport -> Rule.Error {}
reportError functionCallsWithArguments { functionName, position, toError, rangesToRemove } =
    Dict.get functionName functionCallsWithArguments
        |> Maybe.withDefault []
        |> (\callArgumentList -> addArgumentToRemove position callArgumentList rangesToRemove)
        |> List.map Fix.removeRange
        |> toError


addArgumentToRemove : Int -> List (Array a) -> List a -> List a
addArgumentToRemove position callArgumentList acc =
    case callArgumentList of
        [] ->
            acc

        argumentArray :: rest ->
            case Array.get position argumentArray of
                Just range ->
                    addArgumentToRemove position rest (range :: acc)

                Nothing ->
                    -- If an argument at that location could not be found, then we can't autofix the issue.
                    []


findErrorsAndVariablesNotPartOfScope :
    Scope
    -> Declared
    -> { reportLater : Dict Int (List ArgumentToReport), reportNow : List (Rule.Error {}), remainingUsed : Set String }
    -> { reportLater : Dict Int (List ArgumentToReport), reportNow : List (Rule.Error {}), remainingUsed : Set String }
findErrorsAndVariablesNotPartOfScope scope declared { reportLater, reportNow, remainingUsed } =
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
            { reportLater = insertInDictList declared.position (recursiveParameterError scope.functionName declared) reportLater
            , reportNow = reportNow
            , remainingUsed = remainingUsed
            }

    else if Set.member declared.name remainingUsed then
        { reportLater = reportLater
        , reportNow = reportNow
        , remainingUsed = Set.remove declared.name remainingUsed
        }

    else
        case errorsForValue scope.functionName declared of
            ReportNow error ->
                { reportLater = reportLater
                , reportNow = error :: reportNow
                , remainingUsed = remainingUsed
                }

            ReportLater error ->
                { reportLater = insertInDictList declared.position error reportLater
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


findDeclared : Source -> List (Node Pattern) -> Maybe (Node Signature) -> List (List Declared)
findDeclared source arguments signature =
    findDeclaredHelp
        source
        0
        (ParameterPath.fromSignature signature)
        arguments
        []


findDeclaredHelp : Source -> Int -> Path -> List (Node Pattern) -> List (List Declared) -> List (List Declared)
findDeclaredHelp source index path arguments acc =
    case arguments of
        [] ->
            List.reverse acc

        arg :: remainingArguments ->
            findDeclaredHelp
                source
                (index + 1)
                (ParameterPath.nextArgument path)
                remainingArguments
                (getParametersFromPatterns path source arg :: acc)


type ReportTime
    = ReportNow (Rule.Error {})
    | ReportLater ArgumentToReport


errorsForValue : FunctionName -> Declared -> ReportTime
errorsForValue functionName { name, kind, range, source, position, pathInArgument, rangesToRemove, toIgnoredFix } =
    case kind of
        Parameter ->
            let
                toError : List Edit -> Rule.Error {}
                toError =
                    Rule.errorWithFix
                        { message = "Parameter `" ++ name ++ "` is not used"
                        , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                        }
                        range
            in
            case source of
                NamedFunction ->
                    case rangesToRemove of
                        Just rangesToRemove_ ->
                            ReportLater
                                { functionName = functionName
                                , position = position
                                , nesting = pathInArgument
                                , toError = toError
                                , rangesToRemove = rangesToRemove_
                                }

                        Nothing ->
                            ReportNow (toError [])

                Lambda ->
                    ReportNow (toError toIgnoredFix)

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


recursiveParameterError : FunctionName -> Declared -> ArgumentToReport
recursiveParameterError functionName { name, position, pathInArgument, range } =
    { functionName = functionName
    , position = position
    , nesting = pathInArgument
    , toError =
        \edits ->
            -- TODO Support autofixing recursive parameter removal
            Rule.error
                { message = "Parameter `" ++ name ++ "` is only used in recursion"
                , details =
                    [ "This parameter is only used to be passed as an argument to '" ++ functionName ++ "', but its value is never read or used."
                    , "You should either use this parameter somewhere, or remove it at the location I pointed at."
                    ]
                }
                range
    , rangesToRemove = []
    }
