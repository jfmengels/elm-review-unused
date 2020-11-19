module NoUnused.RecordFields exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
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
    { variables : Dict String Variable
    , expressionsToIgnore : Set String
    }


type alias Variable =
    { usedFields : Set String
    , declaredFields : List (Node String)
    , wasUsedWithoutFieldAccess : Bool
    }


initialContext : Context
initialContext =
    { variables = Dict.empty
    , expressionsToIgnore = Set.empty
    }


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor nodes context =
    let
        variables : Dict String Variable
        variables =
            nodes |> List.filterMap registerDeclaration |> Dict.fromList
    in
    ( [], { context | variables = Dict.union variables context.variables } )


registerDeclaration : Node Declaration -> Maybe ( String, Variable )
registerDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            if function.declaration |> Node.value |> .arguments |> List.isEmpty then
                case Node.value function.declaration |> .expression |> Node.value of
                    Expression.RecordExpr fields ->
                        let
                            declaredFields : List (Node String)
                            declaredFields =
                                List.map (Node.value >> Tuple.first) fields
                        in
                        Just
                            ( function.declaration |> Node.value |> .name |> Node.value
                            , { usedFields = Set.empty
                              , declaredFields = declaredFields
                              , wasUsedWithoutFieldAccess = False
                              }
                            )

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing


expressionVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionVisitor node context =
    case Node.value node of
        Expression.RecordAccess (Node functionOrValueRange (Expression.FunctionOrValue [] name)) fieldName ->
            let
                variables : Dict String Variable
                variables =
                    Dict.update name
                        (\maybeDeclared ->
                            case maybeDeclared of
                                Just declared ->
                                    Just { declared | usedFields = Set.insert (Node.value fieldName) declared.usedFields }

                                Nothing ->
                                    Nothing
                        )
                        context.variables
            in
            ( []
            , { context
                | variables = Dict.union variables context.variables
                , expressionsToIgnore = Set.insert (stringifyRange functionOrValueRange) context.expressionsToIgnore
              }
            )

        Expression.FunctionOrValue [] name ->
            if Set.member (stringifyRange (Node.range node)) context.expressionsToIgnore then
                ( [], context )

            else
                let
                    variables : Dict String Variable
                    variables =
                        Dict.update name
                            (\maybeDeclared ->
                                case maybeDeclared of
                                    Just declared ->
                                        Just { declared | wasUsedWithoutFieldAccess = True }

                                    Nothing ->
                                        Nothing
                            )
                            context.variables
                in
                ( []
                , { context | variables = Dict.union variables context.variables }
                )

        _ ->
            ( [], context )


stringifyRange : Range -> String
stringifyRange range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
        |> List.map String.fromInt
        |> String.join "-"


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    context.variables
        |> Dict.toList
        |> List.concatMap (Tuple.second >> finalEvaluationForVariable)


finalEvaluationForVariable : Variable -> List (Error {})
finalEvaluationForVariable variable =
    if variable.wasUsedWithoutFieldAccess then
        []

    else
        variable.declaredFields
            |> List.filter (\node -> not <| Set.member (Node.value node) variable.usedFields)
            |> List.map
                (\node ->
                    Rule.error
                        { message = "Unused field `" ++ Node.value node ++ "`"
                        , details = [ "REPLACEME" ]
                        }
                        (Node.range node)
                )
