module NoUnused.TupleValues exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import NoUnused.NonemptyList as Nonempty exposing (Nonempty)
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
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scopes : Nonempty Scope }


type alias Scope =
    { declared : Dict String (TupleInfo Range)
    , used : Set ( String, Int )
    }


type TupleInfo a
    = TwoTuple ( a, a )
    | ThreeTuple ( a, a, a )


initialContext : Context
initialContext =
    { scopes = Nonempty.fromElement emptyScope }


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
    let
        markUsed : String -> Int -> Pattern -> Context -> Context
        markUsed name index pattern c =
            case pattern of
                Pattern.VarPattern _ ->
                    markTupleValueAsUsed ( name, index ) c

                _ ->
                    c
    in
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
                                        |> markUsed name 0 (Node.value first)
                                        |> markUsed name 1 (Node.value second)

                                ( Pattern.TuplePattern [ first, second, third ], Expression.FunctionOrValue [] name ) ->
                                    context_
                                        |> markUsed name 0 (Node.value first)
                                        |> markUsed name 1 (Node.value second)
                                        |> markUsed name 2 (Node.value third)

                                _ ->
                                    context_
                )
                { context | scopes = Nonempty.cons emptyScope context.scopes }
                declarations
            )

        _ ->
            ( [], context )


markTupleValueAsUsed : ( String, Int ) -> Context -> Context
markTupleValueAsUsed tupleValueKey context =
    let
        scopes : Nonempty Scope
        scopes =
            Nonempty.mapHead
                (\scope ->
                    { scope | used = Set.insert tupleValueKey scope.used }
                )
                context.scopes
    in
    { context | scopes = scopes }


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
        scopes : Nonempty Scope
        scopes =
            Nonempty.mapHead
                (\scope ->
                    { scope | declared = Dict.insert name tupleInfo scope.declared }
                )
                context.scopes
    in
    { context | scopes = scopes }


expressionExitVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression _ ->
            let
                ( errors, usedButNotDeclared ) =
                    makeReport (Nonempty.head context.scopes)

                contextWithPoppedScope : Context
                contextWithPoppedScope =
                    { context | scopes = Nonempty.pop context.scopes }
            in
            ( errors
            , usedButNotDeclared
                |> List.foldl markTupleValueAsUsed contextWithPoppedScope
            )

        _ ->
            ( [], context )


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    Nonempty.head context.scopes
        |> makeReport
        |> Tuple.first


makeReport : Scope -> ( List (Error {}), List ( String, Int ) )
makeReport { declared, used } =
    let
        wasNotUsed : String -> Int -> a -> Maybe a
        wasNotUsed name index value =
            if Set.member ( name, index ) used then
                Nothing

            else
                Just value

        errors : List (Error {})
        errors =
            Dict.foldl
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
                declared

        usedButNotDeclared : Set ( String, Int )
        usedButNotDeclared =
            Set.filter
                (\( tupleName, _ ) ->
                    not (Dict.member tupleName declared)
                )
                used
    in
    ( errors, Set.toList usedButNotDeclared )


error : String -> Range -> Error {}
error name range =
    Rule.error
        { message = "Value in tuple `" ++ name ++ "` is never used."
        , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
        }
        range
