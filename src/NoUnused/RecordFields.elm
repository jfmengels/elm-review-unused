module NoUnused.RecordFields exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)
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
        |> Rule.fromModuleRuleSchema


type alias Context =
    { usedFields : Set String
    , declaredFields : Dict String Range
    }


initialContext : Context
initialContext =
    { usedFields = Set.empty
    , declaredFields = Dict.empty
    }


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor nodes context =
    ( [], context )


expressionVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionVisitor node context =
    ( [], context )
