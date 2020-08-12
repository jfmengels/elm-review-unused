module NoUnused.CustomTypeConstructorArgs exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


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
    Rule.newModuleRuleSchema "NoUnused.CustomTypeConstructorArgs" { customTypeArgs = Dict.empty }
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { customTypeArgs : Dict String (List Range)
    }


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


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    context.customTypeArgs
        |> Dict.toList
        |> List.concatMap Tuple.second
        |> List.map error


error : Range -> Error {}
error range =
    Rule.error
        { message = "REPLACEME"
        , details = [ "REPLACEME" ]
        }
        range
