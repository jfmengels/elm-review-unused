module NoUnused.TupleValues exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


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
    Rule.newModuleRuleSchema "NoUnused.TupleValues" initialContext
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scope : Scope }


type alias Scope =
    { declared : Dict String (TupleInfo Range)
    , used : Set ( String, Int )
    }


type TupleInfo a
    = TwoTuple ( a, a )
    | ThreeTuple ( a, a, a )


initialContext : Context
initialContext =
    { scope = emptyScope }


emptyScope : Scope
emptyScope =
    { declared = Dict.empty
    , used = Set.empty
    }


declarationEnterVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( []
            , function.signature
                |> Maybe.map (registerTupleSignature context)
                |> Maybe.withDefault context
            )

        _ ->
            ( [], context )


expressionEnterVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            ( []
            , List.foldl
                (\declaration context_ ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            function.signature
                                |> Maybe.map (registerTupleSignature context_)
                                |> Maybe.withDefault context_

                        Expression.LetDestructuring patternNode expressionNode ->
                            case ( Node.value patternNode, Node.value expressionNode ) of
                                ( Pattern.TuplePattern [ first, second ], Expression.FunctionOrValue [] name ) ->
                                    context_
                                        |> markTupleValueAsUsed name 0 (Node.value first)
                                        |> markTupleValueAsUsed name 1 (Node.value second)

                                ( Pattern.TuplePattern [ first, second, third ], Expression.FunctionOrValue [] name ) ->
                                    context_
                                        |> markTupleValueAsUsed name 0 (Node.value first)
                                        |> markTupleValueAsUsed name 1 (Node.value second)
                                        |> markTupleValueAsUsed name 2 (Node.value third)

                                _ ->
                                    context_
                )
                context
                declarations
            )

        _ ->
            ( [], context )


markTupleValueAsUsed : String -> Int -> Pattern.Pattern -> Context -> Context
markTupleValueAsUsed name index pattern context =
    let
        scope : Scope
        scope =
            context.scope

        newScope : Scope
        newScope =
            case pattern of
                Pattern.VarPattern _ ->
                    { scope | used = Set.insert ( name, index ) scope.used }

                _ ->
                    scope
    in
    { context | scope = newScope }


registerTupleSignature : Context -> Node Signature -> Context
registerTupleSignature context signatureNode =
    let
        signature : Signature
        signature =
            Node.value signatureNode

        name : String
        name =
            Node.value signature.name

        typeAnnotation : TypeAnnotation
        typeAnnotation =
            Node.value signature.typeAnnotation
    in
    case typeAnnotation of
        TypeAnnotation.Tupled [ first, second ] ->
            registerTuple
                (TwoTuple ( Node.range first, Node.range second ))
                name
                context

        TypeAnnotation.Tupled [ first, second, third ] ->
            registerTuple
                (ThreeTuple ( Node.range first, Node.range second, Node.range third ))
                name
                context

        _ ->
            context


registerTuple : TupleInfo Range -> String -> Context -> Context
registerTuple tupleInfo name context =
    let
        scope : Scope
        scope =
            context.scope

        newScope : Scope
        newScope =
            { scope | declared = Dict.insert name tupleInfo scope.declared }
    in
    { context | scope = newScope }


expressionExitVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression _ ->
            ( makeReport context.scope, context )

        _ ->
            ( [], context )


makeReport : Scope -> List (Error {})
makeReport { declared, used } =
    let
        wasNotUsed : String -> Int -> a -> Maybe a
        wasNotUsed name index value =
            if Set.member ( name, index ) used then
                Nothing

            else
                Just value
    in
    declared
        |> Dict.foldl
            (\tupleName tupleInfo acc ->
                let
                    errorRanges : List Range
                    errorRanges =
                        case tupleInfo of
                            TwoTuple ( first, second ) ->
                                List.filterMap identity
                                    [ wasNotUsed tupleName 0 first
                                    , wasNotUsed tupleName 1 second
                                    ]

                            ThreeTuple ( first, second, third ) ->
                                List.filterMap identity
                                    [ wasNotUsed tupleName 0 first
                                    , wasNotUsed tupleName 1 second
                                    , wasNotUsed tupleName 2 third
                                    ]
                in
                List.map (error tupleName) errorRanges ++ acc
            )
            []


error : String -> Range -> Error {}
error name range =
    Rule.error
        { message = "Value in tuple `" ++ name ++ "` is never used."
        , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
        }
        range
