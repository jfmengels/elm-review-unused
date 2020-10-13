module NoUnused.TupleValues exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Review.Rule as Rule exposing (Rule)


{-| Reports tuple values that are never used.

    config =
        [ NoUnused.TupleValues.rule
        ]

    TODO: write me


## Fail

    numberAndBool : ( Int, Bool )
    numberAndBool =
        ( 1, True )

    result : Int
    result =
        let
            ( first, _ ) =
                numberAndBool
        in
        first


## Success

    twoNumbers : ( Int, Int )
    twoNumbers =
        ( 1, 2 )

    result : Int
    result =
        let
            ( first, second ) =
                twoNumbers
        in
        first + second


## When not to enable this rule?

TODO: write me


## Try it out

You can try this rule out by running the following command:

```bash
elm - review --template jfmengels/elm-review-unused/example --rules NoUnused.TupleValues
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnused.TupleValues" ()
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    ()


expressionEnterVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            case declarations of
                [] ->
                    ( [], context )

                first :: _ ->
                    case Node.value first of
                        Expression.LetFunction function ->
                            case function.signature of
                                Just signature ->
                                    case Node.value (Node.value signature).typeAnnotation of
                                        TypeAnnotation.Tupled (ft :: _) ->
                                            ( [ Rule.error
                                                    { message = "Tuple value is never used."
                                                    , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
                                                    }
                                                    (Node.range ft)
                                              ]
                                            , context
                                            )

                                        _ ->
                                            ( [], context )

                                Nothing ->
                                    ( [], context )

                        _ ->
                            ( [], context )

        Expression.TupledExpression _ ->
            ( [], context )

        _ ->
            ( [], context )
