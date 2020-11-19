module NoUnused.RecordFields exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


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
    Rule.newModuleRuleSchema "NoUnused.RecordFields" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    Dict String Variable


type alias Variable =
    { usedFields : Set String
    , declaredFields : Dict String Range
    , wasUsedWithoutFieldAccess : Bool
    }


initialContext : Context
initialContext =
    Dict.empty


newVariable : Variable
newVariable =
    { usedFields = Set.singleton "foo"
    , declaredFields =
        Dict.fromList
            [ ( "foo", Range.emptyRange )
            , ( "unused", { start = { row = 2, column = 13 }, end = { row = 2, column = 19 } } )
            ]
    , wasUsedWithoutFieldAccess = True
    }


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor nodes context =
    let
        variables : Dict String Variable
        variables =
            nodes |> List.filterMap registerDeclaration |> Dict.fromList
    in
    ( [], Dict.union variables context )


registerDeclaration : Node Declaration -> Maybe ( String, Variable )
registerDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            Just ( "a", newVariable )

        _ ->
            Nothing


expressionVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionVisitor node context =
    ( [], context )


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    context
        |> Dict.toList
        |> List.concatMap (Tuple.second >> finalEvaluationForVariable)


finalEvaluationForVariable : Variable -> List (Error {})
finalEvaluationForVariable variable =
    if variable.wasUsedWithoutFieldAccess then
        []

    else
        variable.declaredFields
            |> Dict.toList
            |> List.filter (\( fieldName, _ ) -> not <| Set.member fieldName variable.usedFields)
            |> List.map
                (\( fieldName, range ) ->
                    Rule.error
                        { message = "Unused field `" ++ fieldName ++ "`"
                        , details = [ "REPLACEME" ]
                        }
                        range
                )
