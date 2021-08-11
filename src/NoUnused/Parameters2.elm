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
    , source : Source
    , fix : List Fix
    }


type Kind
    = Parameter
    | Alias
    | AsWithoutVariables
    | TupleWithoutVariables


type Source
    = NamedFunction
    | Lambda


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
                        |> List.concatMap (getParametersFromPatterns NamedFunction)
            in
            ( []
            , { context
                | scopes = []
                , scopesToCreate =
                    RangeDict.singleton
                        (declaration |> Node.value |> .expression |> Node.range)
                        declared
              }
            )

        _ ->
            ( [], context )


getParametersFromPatterns : Source -> Node Pattern -> List Declared
getParametersFromPatterns source node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            getParametersFromPatterns source pattern

        Pattern.VarPattern name ->
            [ { name = name
              , range = Node.range node
              , kind = Parameter
              , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
              , source = source
              }
            ]

        Pattern.AsPattern pattern asName ->
            let
                parametersFromPatterns : List Declared
                parametersFromPatterns =
                    getParametersFromPatterns source pattern

                asParameter : Declared
                asParameter =
                    { name = Node.value asName
                    , range = Node.range asName
                    , kind = Alias
                    , fix = [ Fix.removeRange { start = (Node.range pattern).end, end = (Node.range asName).end } ]
                    , source = source
                    }
            in
            if List.isEmpty parametersFromPatterns then
                [ asParameter
                , { name = ""
                  , range = Node.range pattern
                  , kind = AsWithoutVariables
                  , fix = [ Fix.removeRange { start = (Node.range pattern).start, end = (Node.range asName).start } ]
                  , source = source
                  }
                ]

            else
                asParameter :: parametersFromPatterns

        Pattern.RecordPattern fields ->
            List.map
                (\field ->
                    { name = Node.value field
                    , range = Node.range field
                    , kind = Parameter
                    , fix = []
                    , source = source
                    }
                )
                fields

        Pattern.TuplePattern patterns ->
            let
                parametersFromPatterns : List Declared
                parametersFromPatterns =
                    List.concatMap (getParametersFromPatterns source) patterns
            in
            if List.isEmpty parametersFromPatterns && List.all isPatternWildCard patterns then
                [ { name = ""
                  , range = Node.range node
                  , kind = TupleWithoutVariables
                  , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                  , source = source
                  }
                ]

            else
                parametersFromPatterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap (getParametersFromPatterns source) patterns

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

        Expression.RecordUpdateExpression name _ ->
            ( [], markValueAsUsed (Node.value name) context )

        Expression.LetExpression letBlock ->
            let
                declaredWithRange : List ( Range, List Declared )
                declaredWithRange =
                    List.map
                        (\letDeclaration ->
                            case Node.value letDeclaration of
                                Expression.LetFunction function ->
                                    let
                                        declaration : Expression.FunctionImplementation
                                        declaration =
                                            Node.value function.declaration
                                    in
                                    ( Node.range declaration.expression
                                    , List.concatMap (getParametersFromPatterns NamedFunction) declaration.arguments
                                    )

                                Expression.LetDestructuring pattern expression ->
                                    ( Node.range expression, [] )
                        )
                        letBlock.declarations

                scopesToCreate : RangeDict (List Declared)
                scopesToCreate =
                    List.foldl
                        (\( range, declared ) dict ->
                            RangeDict.insert
                                range
                                declared
                                dict
                        )
                        context.scopesToCreate
                        declaredWithRange
            in
            ( [], { context | scopesToCreate = scopesToCreate } )

        Expression.LambdaExpression { args, expression } ->
            let
                scopesToCreate : RangeDict (List Declared)
                scopesToCreate =
                    RangeDict.insert
                        (Node.range expression)
                        (List.concatMap (getParametersFromPatterns Lambda) args)
                        context.scopesToCreate
            in
            ( [], { context | scopesToCreate = scopesToCreate } )

        _ ->
            ( [], context )


markValueAsUsed : String -> Context -> Context
markValueAsUsed name context =
    case context.scopes of
        [] ->
            context

        headScope :: restOfScopes ->
            { context | scopes = { headScope | used = Set.insert name headScope.used } :: restOfScopes }


markAllAsUsed : Set String -> List Scope -> List Scope
markAllAsUsed names scopes =
    case scopes of
        [] ->
            scopes

        headScope :: restOfScopes ->
            { headScope | used = Set.union names headScope.used } :: restOfScopes



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
                ( errors, remainingUsed ) =
                    List.foldl
                        (\declared ( errors_, remainingUsed_ ) ->
                            if Set.member declared.name remainingUsed_ then
                                ( errors_, Set.remove declared.name remainingUsed_ )

                            else
                                ( errorsForValue declared :: errors_, remainingUsed_ )
                        )
                        ( [], headScope.used )
                        headScope.declared
            in
            ( errors, { context | scopes = markAllAsUsed remainingUsed restOfScopes } )

        [] ->
            ( [], context )


errorsForValue : Declared -> Rule.Error {}
errorsForValue { name, kind, range, source, fix } =
    case kind of
        Parameter ->
            Rule.errorWithFix
                { message = "Parameter `" ++ name ++ "` is not used."
                , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                }
                range
                (applyFix source fix)

        Alias ->
            Rule.errorWithFix
                { message = "Pattern alias `" ++ name ++ "` is not used."
                , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                }
                range
                (applyFix source fix)

        AsWithoutVariables ->
            Rule.errorWithFix
                { message = "Pattern does not introduce any variable"
                , details = [ "You should remove this pattern." ]
                }
                range
                (applyFix source fix)

        TupleWithoutVariables ->
            Rule.errorWithFix
                { message = "Tuple pattern is not needed"
                , details = [ "You should remove this pattern." ]
                }
                range
                (applyFix source fix)


applyFix : Source -> List Fix -> List Fix
applyFix source fix =
    case source of
        NamedFunction ->
            []

        Lambda ->
            fix
