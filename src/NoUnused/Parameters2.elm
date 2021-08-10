module NoUnused.Parameters2 exposing (rule)

{-| Report parameters that are not used.


# Rule

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Writer as Writer
import NoUnused.Patterns.NameVisitor as NameVisitor
import NoUnused.RangeDict as RangeDict exposing (RangeDict)
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

Value `something` is not used:

    add1 number =
        1


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
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema



--- CONTEXT


type alias Context =
    { scopes : List Scope
    , scopesToCreate : RangeDict (List Declared)
    }


type alias Scope =
    { declared : List Declared
    , used : Set String
    }


type alias Declared =
    { name : String
    , range : Range
    , kind : Kind
    , fix : List Fix
    }


type Kind
    = Parameter
    | Alias
    | AsWithPatternWithoutVariables
    | TupleWithoutVariables


type FoundPattern
    = SingleValue
        { name : String
        , range : Range
        , message : String
        , details : List String
        , fix : List Fix
        }
    | RecordPattern
        { fields : List (Node String)
        , recordRange : Range
        }


initialContext : Context
initialContext =
    { scopes = []
    , scopesToCreate = RangeDict.empty
    }



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                declared : List Declared
                declared =
                    Node.value declaration
                        |> .arguments
                        |> List.concatMap getParametersFromPatterns
            in
            ( []
            , { context
                | scopes = []
                , scopesToCreate =
                    RangeDict.insert
                        (declaration |> Node.value |> .expression |> Node.range)
                        declared
                        context.scopesToCreate
              }
            )

        _ ->
            ( [], context )


getParametersFromPatterns : Node Pattern -> List Declared
getParametersFromPatterns node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            getParametersFromPatterns pattern

        Pattern.VarPattern name ->
            [ { name = name, range = Node.range node, kind = Parameter, fix = [] } ]

        Pattern.AsPattern pattern asName ->
            let
                parametersFromPatterns : List Declared
                parametersFromPatterns =
                    getParametersFromPatterns pattern

                asParameter : Declared
                asParameter =
                    { name = Node.value asName, range = Node.range asName, kind = Alias, fix = [] }
            in
            if List.isEmpty parametersFromPatterns then
                [ asParameter
                , { name = ""
                  , range = Node.range pattern
                  , kind = AsWithPatternWithoutVariables
                  , fix = [ Fix.removeRange { start = (Node.range pattern).start, end = (Node.range asName).start } ]
                  }
                ]

            else
                asParameter :: parametersFromPatterns

        Pattern.RecordPattern fields ->
            List.map
                (\field ->
                    { name = Node.value field, range = Node.range field, kind = Parameter, fix = [] }
                )
                fields

        Pattern.TuplePattern patterns ->
            let
                parametersFromPatterns : List Declared
                parametersFromPatterns =
                    List.concatMap getParametersFromPatterns patterns
            in
            if List.isEmpty parametersFromPatterns && List.all isPatternWildCard patterns then
                [ { name = ""
                  , range = Node.range node
                  , kind = TupleWithoutVariables
                  , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                  }
                ]

            else
                parametersFromPatterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap getParametersFromPatterns patterns

        _ ->
            []


isPatternWildCard : Node Pattern -> Bool
isPatternWildCard node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            isPatternWildCard pattern

        Pattern.AllPattern ->
            True

        Pattern.UnitPattern ->
            True

        _ ->
            False



-- EXPRESSION ENTER VISITOR


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    let
        newContext : Context
        newContext =
            case RangeDict.get (Node.range node) context.scopesToCreate of
                Just declared ->
                    { context | scopes = { declared = declared, used = Set.empty } :: context.scopes }

                Nothing ->
                    context
    in
    expressionEnterVisitorHelp node newContext


expressionEnterVisitorHelp : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            ( [], markValueAsUsed name context )

        _ ->
            ( [], context )


markValueAsUsed : String -> Context -> Context
markValueAsUsed name context =
    case context.scopes of
        [] ->
            context

        headScope :: restOfScopes ->
            { context | scopes = { headScope | used = Set.insert name headScope.used } :: restOfScopes }



-- EXPRESSION EXIT VISITOR


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    case RangeDict.get (Node.range node) context.scopesToCreate of
        Just _ ->
            report context

        Nothing ->
            ( [], context )


report : Context -> ( List (Rule.Error {}), Context )
report context =
    case context.scopes of
        headScope :: restOfScopes ->
            let
                errors : List (Rule.Error {})
                errors =
                    headScope.declared
                        |> List.filter (\{ name } -> not (Set.member name headScope.used))
                        |> List.map errorsForValue
            in
            ( errors, { context | scopes = restOfScopes } )

        [] ->
            ( [], context )


errorsForValue : Declared -> Rule.Error {}
errorsForValue { name, kind, range, fix } =
    case kind of
        Parameter ->
            Rule.error
                { message = "Parameter `" ++ name ++ "` is not used."
                , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                }
                range

        Alias ->
            Rule.error
                { message = "Pattern alias `" ++ name ++ "` is not used."
                , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                }
                range

        AsWithPatternWithoutVariables ->
            Rule.errorWithFix
                { message = "Pattern does not introduce any variable"
                , details = [ "You should remove this pattern." ]
                }
                range
                fix

        TupleWithoutVariables ->
            Rule.errorWithFix
                { message = "Tuple pattern is not needed"
                , details = [ "You should remove this pattern." ]
                }
                range
                fix


listToMessage : String -> List String -> String
listToMessage first rest =
    case List.reverse rest of
        [] ->
            "Parameter `" ++ first ++ "` is not used."

        last :: middle ->
            "Parameters `" ++ String.join "`, `" (first :: middle) ++ "` and `" ++ last ++ "` are not used."
