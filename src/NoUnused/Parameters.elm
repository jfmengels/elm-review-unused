module NoUnused.Parameters exposing (rule)

{-| Report parameters that are not used.

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import Review.Fix as Fix exposing (Fix)
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
    Rule.newModuleRuleSchema "NoUnused.Parameters" initialContext
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withLetDeclarationEnterVisitor letDeclarationEnterVisitor
        |> Rule.withLetDeclarationExitVisitor letDeclarationExitVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema



--- CONTEXT


type alias Context =
    { scopes : Nonempty Scope
    , knownFunctions : Dict String FunctionArgs
    , locationsToIgnoreForUsed : LocationsToIgnore
    }


type alias Scope =
    { functionName : FunctionName
    , declared : List Declared
    , used : Set String
    , usedRecursively : Set String
    , toReport : ToReport
    }


type alias FunctionName =
    String


type alias ToReport =
    Dict FunctionName (Dict Int (List (Rule.Error {})))


type alias Declared =
    { name : String
    , range : Range
    , kind : Kind
    , source : Source
    , position : Int
    , fix : List Fix
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


initialContext : Context
initialContext =
    { scopes =
        NonemptyList.fromElement
            { functionName = "root"
            , declared = []
            , used = Set.empty
            , usedRecursively = Set.empty
            , toReport = Dict.empty
            }
    , knownFunctions = Dict.empty
    , locationsToIgnoreForUsed = Dict.empty
    }



-- DECLARATION VISITOR


declarationEnterVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                arguments : List (Node Pattern)
                arguments =
                    (Node.value declaration).arguments

                declared : List (List Declared)
                declared =
                    List.indexedMap (\index arg -> getParametersFromPatterns index NamedFunction arg) arguments

                functionName : FunctionName
                functionName =
                    Node.value declaration |> .name |> Node.value
            in
            ( []
            , { scopes =
                    NonemptyList.cons
                        { functionName = functionName
                        , declared = List.concat declared
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = Dict.empty
                        }
                        context.scopes
              , knownFunctions = Dict.singleton functionName (getArgNames declared)
              , locationsToIgnoreForUsed = Dict.empty
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


getParametersFromPatterns : Int -> Source -> Node Pattern -> List Declared
getParametersFromPatterns index source node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            getParametersFromPatterns index source pattern

        Pattern.VarPattern name ->
            [ { name = name
              , range = Node.range node
              , kind = Parameter
              , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
              , position = index
              , source = source
              }
            ]

        Pattern.AsPattern pattern asName ->
            getParametersFromAsPattern index source pattern asName

        Pattern.RecordPattern fields ->
            case fields of
                [ field ] ->
                    [ { name = Node.value field
                      , range = Node.range field
                      , kind = Parameter
                      , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                      , position = index
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
                            , fix =
                                [ Fix.replaceRangeBy
                                    (Node.range node)
                                    (fieldNames |> List.filter (\f -> f /= Node.value field) |> formatRecord)
                                ]
                            , position = index
                            , source = source
                            }
                        )
                        fields

        Pattern.TuplePattern patterns ->
            let
                parametersFromPatterns : List Declared
                parametersFromPatterns =
                    List.concatMap (getParametersFromPatterns index source) patterns
            in
            if List.isEmpty parametersFromPatterns && List.all isPatternWildCard patterns then
                [ { name = ""
                  , range = Node.range node
                  , kind = TupleWithoutVariables
                  , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                  , position = index
                  , source = source
                  }
                ]

            else
                parametersFromPatterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap (getParametersFromPatterns index source) patterns

        _ ->
            []


getParametersFromAsPattern : Int -> Source -> Node Pattern -> Node String -> List Declared
getParametersFromAsPattern index source pattern asName =
    let
        parametersFromPatterns : List Declared
        parametersFromPatterns =
            getParametersFromPatterns index source pattern

        asParameter : Declared
        asParameter =
            { name = Node.value asName
            , range = Node.range asName
            , kind = Alias
            , fix = [ Fix.removeRange { start = (Node.range pattern).end, end = (Node.range asName).end } ]
            , position = index
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


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    ( [], expressionEnterVisitorHelp node context )


expressionEnterVisitorHelp : Node Expression -> Context -> Context
expressionEnterVisitorHelp node context =
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
                        , declared = List.indexedMap (\index arg -> getParametersFromPatterns index Lambda arg) args |> List.concat
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = Dict.empty
                        }
                        context.scopes
            }

        Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments) ->
            registerFunctionCall fnName 0 arguments context

        Expression.OperatorApplication "|>" _ lastArgument (Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments))) ->
            -- Ignoring "arguments" because they will be visited when the Application node will be visited anyway.
            registerFunctionCall fnName (List.length arguments) [ lastArgument ] context

        Expression.OperatorApplication "<|" _ (Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments))) lastArgument ->
            -- Ignoring "arguments" because they will be visited when the Application node will be visited anyway.
            registerFunctionCall fnName (List.length arguments) [ lastArgument ] context

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
                        List.indexedMap (\index arg -> getParametersFromPatterns index NamedFunction arg) declaration.arguments

                    newScope : Scope
                    newScope =
                        { functionName = functionName
                        , declared = List.concat declared
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = Dict.empty
                        }
                in
                ( []
                , { context
                    | scopes = NonemptyList.cons newScope context.scopes
                    , knownFunctions = Dict.insert functionName (getArgNames declared) context.knownFunctions
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


registerFunctionCall : String -> Int -> List (Node a) -> Context -> Context
registerFunctionCall fnName numberOfIgnoredArguments arguments context =
    case Dict.get fnName context.knownFunctions of
        Just fnArgs ->
            let
                locationsToIgnore : LocationsToIgnore
                locationsToIgnore =
                    ignoreLocations fnArgs numberOfIgnoredArguments arguments 0 context.locationsToIgnoreForUsed
            in
            { context | locationsToIgnoreForUsed = locationsToIgnore }

        Nothing ->
            context


ignoreLocations : FunctionArgs -> Int -> List (Node a) -> Int -> LocationsToIgnore -> LocationsToIgnore
ignoreLocations fnArgs numberOfIgnoredArguments nodes index acc =
    case nodes of
        [] ->
            acc

        (Node range _) :: rest ->
            let
                newAcc : LocationsToIgnore
                newAcc =
                    case Dict.get (numberOfIgnoredArguments + index) fnArgs of
                        Just argName ->
                            insertInDictList argName range acc

                        Nothing ->
                            acc
            in
            ignoreLocations fnArgs numberOfIgnoredArguments rest (index + 1) newAcc


finalEvaluation : Context -> List (Rule.Error {})
finalEvaluation context =
    (NonemptyList.head context.scopes).toReport
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concat


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
    case Dict.get name context.locationsToIgnoreForUsed of
        Just ranges ->
            List.any (isRangeIncluded range) ranges

        Nothing ->
            False


isRangeIncluded : Range -> Range -> Bool
isRangeIncluded inner outer =
    (Range.compareLocations inner.start outer.start /= LT)
        && (Range.compareLocations inner.end outer.end /= GT)


markAllAsUsed : Set String -> FunctionName -> Dict Int (List (Rule.Error {})) -> Nonempty Scope -> Nonempty Scope
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

        ( toReport, remainingUsed ) =
            List.foldl
                (findErrorsAndVariablesNotPartOfScope headScope)
                ( Dict.empty, headScope.used )
                headScope.declared
    in
    ( headScope.toReport
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.concat
    , { context
        | scopes = markAllAsUsed remainingUsed headScope.functionName toReport scopes
        , knownFunctions = Dict.remove headScope.functionName context.knownFunctions
      }
    )


findErrorsAndVariablesNotPartOfScope : Scope -> Declared -> ( Dict Int (List (Rule.Error {})), Set String ) -> ( Dict Int (List (Rule.Error {})), Set String )
findErrorsAndVariablesNotPartOfScope scope declared ( errors_, remainingUsed ) =
    if Set.member declared.name scope.usedRecursively then
        -- If variable was used as a recursive argument
        if Set.member declared.name remainingUsed then
            -- If variable was used somewhere else as well
            ( errors_, Set.remove declared.name remainingUsed )

        else
            -- If variable was used ONLY as a recursive argument
            ( insertInDictList declared.position (recursiveParameterError scope.functionName declared) errors_, remainingUsed )

    else if Set.member declared.name remainingUsed then
        ( errors_, Set.remove declared.name remainingUsed )

    else
        ( insertInDictList declared.position (errorsForValue declared) errors_, remainingUsed )


insertInDictList : comparable -> value -> Dict comparable (List value) -> Dict comparable (List value)
insertInDictList key value dict =
    case Dict.get key dict of
        Nothing ->
            Dict.insert key [ value ] dict

        Just previous ->
            Dict.insert key (value :: previous) dict


errorsForValue : Declared -> Rule.Error {}
errorsForValue { name, kind, range, source, fix } =
    case kind of
        Parameter ->
            Rule.errorWithFix
                { message = "Parameter `" ++ name ++ "` is not used"
                , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                }
                range
                (applyFix source fix)

        Alias ->
            Rule.errorWithFix
                { message = "Pattern alias `" ++ name ++ "` is not used"
                , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                }
                range
                fix

        TupleWithoutVariables ->
            Rule.errorWithFix
                { message = "Tuple pattern is not needed"
                , details = [ "You should remove this pattern." ]
                }
                range
                (applyFix source fix)


recursiveParameterError : FunctionName -> Declared -> Rule.Error {}
recursiveParameterError functionName { name, range } =
    Rule.error
        { message = "Parameter `" ++ name ++ "` is only used in recursion"
        , details =
            [ "This parameter is only used to be passed as an argument to '" ++ functionName ++ "', but its value is never read or used."
            , "You should either use this parameter somewhere, or remove it at the location I pointed at."
            ]
        }
        range


applyFix : Source -> List Fix -> List Fix
applyFix source fix =
    case source of
        NamedFunction ->
            []

        Lambda ->
            fix
