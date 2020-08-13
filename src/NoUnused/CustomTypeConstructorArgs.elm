module NoUnused.CustomTypeConstructorArgs exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports... REPLACEME

    config =
        [ NoUnused.CustomTypeConstructorArgs.rule
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
elm - review --template jfmengels/elm-review-unused/example --rules NoUnused.CustomTypeConstructorArgs
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnused.CustomTypeConstructorArgs" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { customTypeArgs : Dict String (List Range)
    , usedArguments : Dict ( ModuleName, String ) (Set Int)
    }


initialContext : Context
initialContext =
    { customTypeArgs = Dict.empty
    , usedArguments = Dict.empty
    }



-- DECLARATION VISITOR


declarationListVisitor : List (Node Declaration) -> Context -> ( List (Error {}), Context )
declarationListVisitor nodes context =
    let
        customTypeArgs : List ( String, List Range )
        customTypeArgs =
            List.concatMap collectCustomType nodes
    in
    ( [], { context | customTypeArgs = Dict.fromList customTypeArgs } )


collectCustomType : Node Declaration -> List ( String, List Range )
collectCustomType node =
    case Node.value node of
        Declaration.CustomTypeDeclaration { constructors } ->
            constructors
                |> List.map (Node.value >> (\{ name, arguments } -> ( Node.value name, List.map Node.range arguments )))

        _ ->
            []



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            let
                arguments : List ( ( ModuleName, String ), Set Int )
                arguments =
                    cases
                        |> List.concatMap (Tuple.first >> detectUsedPatterns)

                newUsedArguments : Dict ( ModuleName, String ) (Set Int)
                newUsedArguments =
                    List.foldl
                        (\( key, usedPositions ) acc ->
                            let
                                previouslyUsedPositions : Set Int
                                previouslyUsedPositions =
                                    Dict.get key acc
                                        |> Maybe.withDefault Set.empty
                            in
                            Dict.insert key (Set.union previouslyUsedPositions usedPositions) acc
                        )
                        context.usedArguments
                        arguments
            in
            ( [], { context | usedArguments = newUsedArguments } )

        _ ->
            ( [], context )


detectUsedPatterns : Node Pattern -> List ( ( ModuleName, String ), Set Int )
detectUsedPatterns (Node _ pattern) =
    case pattern of
        Pattern.NamedPattern { moduleName, name } args ->
            let
                usedPositions : Set Int
                usedPositions =
                    args
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, subPattern ) -> not <| isWildcard subPattern)
                        |> List.map Tuple.first
                        |> Set.fromList
            in
            [ ( ( moduleName, name ), usedPositions ) ]
                ++ List.concatMap detectUsedPatterns args

        Pattern.TuplePattern patterns ->
            List.concatMap detectUsedPatterns patterns

        Pattern.ListPattern patterns ->
            List.concatMap detectUsedPatterns patterns

        Pattern.UnConsPattern left right ->
            List.concatMap detectUsedPatterns [ left, right ]

        Pattern.ParenthesizedPattern subPattern ->
            detectUsedPatterns subPattern

        _ ->
            []


isWildcard : Node Pattern -> Bool
isWildcard node =
    case Node.value node of
        Pattern.AllPattern ->
            True

        Pattern.ParenthesizedPattern pattern ->
            isWildcard pattern

        _ ->
            False



-- FINAL EVALUATION


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    context.customTypeArgs
        |> Dict.toList
        |> List.concatMap
            (\( name, arguments ) ->
                case Dict.get ( [], name ) context.usedArguments of
                    Just usedArgumentPositions ->
                        arguments
                            |> List.indexedMap Tuple.pair
                            |> List.filter (\( index, _ ) -> not <| Set.member index usedArgumentPositions)
                            |> List.map Tuple.second

                    Nothing ->
                        arguments
            )
        |> List.map error


error : Range -> Error {}
error range =
    Rule.error
        { message = "REPLACEME"
        , details = [ "REPLACEME" ]
        }
        range
