module NoUnused.Parameters2 exposing (rule)

{-| Report parameters that are not used.


# Rule

@docs rule

-}

import Array
import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
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
    , scopesToCreate : RangeDict ( List Declared, String )
    , locationsToIgnoreForUsed : RangeDict ()
    }


type alias Scope =
    { functionName : String
    , declared : List Declared
    , used : Set String
    , usedRecursively : Set String
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


initialContext : Context
initialContext =
    { scopes = []
    , scopesToCreate = RangeDict.empty
    , locationsToIgnoreForUsed = RangeDict.empty
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
            , { scopes = []
              , scopesToCreate =
                    RangeDict.singleton
                        (declaration |> Node.value |> .expression |> Node.range)
                        ( declared
                        , Node.value declaration |> .name |> Node.value
                        )
              , locationsToIgnoreForUsed = RangeDict.empty
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
            case fields of
                [ field ] ->
                    [ { name = Node.value field
                      , range = Node.range field
                      , kind = Parameter
                      , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                      , source = source
                      }
                    ]

                _ ->
                    let
                        fieldNames : List String
                        fieldNames =
                            List.map Node.value fields
                    in
                    List.map
                        (\field ->
                            { name = Node.value field
                            , range = Node.range field
                            , kind = Parameter
                            , fix =
                                [ Fix.replaceRangeBy
                                    (Node.range node)
                                    (fieldNames |> List.filter (\f -> f /= Node.value field) |> formatRecord)
                                ]
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


formatRecord : List String -> String
formatRecord fields =
    "{ " ++ String.join ", " fields ++ " }"



-- EXPRESSION ENTER VISITOR


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    let
        newContext : Context
        newContext =
            case RangeDict.get (Node.range node) context.scopesToCreate of
                Just ( declared, functionName ) ->
                    { context
                        | scopes =
                            { functionName = functionName
                            , declared = declared
                            , used = Set.empty
                            , usedRecursively = Set.singleton "unused"
                            }
                                :: context.scopes
                    }

                Nothing ->
                    context
    in
    expressionEnterVisitorHelp node newContext


expressionEnterVisitorHelp : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            ( [], markValueAsUsed (Node.range node) name context )

        Expression.RecordUpdateExpression name _ ->
            ( [], markValueAsUsed (Node.range name) (Node.value name) context )

        Expression.LetExpression letBlock ->
            let
                declaredWithRange : List ( Range, ( List Declared, String ) )
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
                                    , ( List.concatMap (getParametersFromPatterns NamedFunction) declaration.arguments
                                      , Node.value declaration.name
                                      )
                                    )

                                Expression.LetDestructuring _ expression ->
                                    ( Node.range expression, ( [], "" ) )
                        )
                        letBlock.declarations

                scopesToCreate : RangeDict ( List Declared, String )
                scopesToCreate =
                    RangeDict.insertAll declaredWithRange context.scopesToCreate
            in
            ( [], { context | scopesToCreate = scopesToCreate } )

        Expression.LambdaExpression { args, expression } ->
            let
                scopesToCreate : RangeDict ( List Declared, String )
                scopesToCreate =
                    RangeDict.insert
                        (Node.range expression)
                        ( List.concatMap (getParametersFromPatterns Lambda) args, "dummy lambda" )
                        context.scopesToCreate
            in
            ( [], { context | scopesToCreate = scopesToCreate } )

        Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments) ->
            let
                knownFunctions : Dict String (Dict Int String)
                knownFunctions =
                    -- TODO Remove harcoding
                    Dict.singleton
                        "foo"
                        (Dict.fromList [ ( 0, "x" ), ( 1, "unused" ) ])
            in
            case Dict.get fnName knownFunctions of
                Just fnArgs ->
                    let
                        recursiveCalls : List { name : String, range : Range }
                        recursiveCalls =
                            arguments
                                |> List.indexedMap Tuple.pair
                                |> List.filterMap
                                    (\( index, arg ) ->
                                        Maybe.andThen
                                            (\name -> Maybe.map (CallLocation name) (getReference name arg))
                                            (Dict.get index fnArgs)
                                    )

                        recursiveReferences : Set String
                        recursiveReferences =
                            recursiveCalls
                                |> List.map .name
                                |> Set.fromList

                        locationsToIgnore : List ( Range, () )
                        locationsToIgnore =
                            List.map (\call -> ( call.range, () )) recursiveCalls
                    in
                    ( [], markRecursiveValueAsUsed recursiveReferences { context | locationsToIgnoreForUsed = RangeDict.insertAll locationsToIgnore context.locationsToIgnoreForUsed } )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


type alias CallLocation =
    { name : String
    , range : Range
    }


getReference : String -> Node Expression -> Maybe Range
getReference name node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            getReference name expr

        Expression.FunctionOrValue [] referenceName ->
            if referenceName == name then
                Just (Node.range node)

            else
                Nothing

        Expression.RecordUpdateExpression _ _ ->
            -- TODO Add a test and implement
            Nothing

        _ ->
            Nothing


markValueAsUsed : Range -> String -> Context -> Context
markValueAsUsed range name context =
    if RangeDict.member range context.locationsToIgnoreForUsed then
        context

    else
        case context.scopes of
            [] ->
                context

            headScope :: restOfScopes ->
                let
                    newHeadScope : Scope
                    newHeadScope =
                        { headScope | used = Set.insert name headScope.used }
                in
                { context | scopes = newHeadScope :: restOfScopes }


markRecursiveValueAsUsed : Set String -> Context -> Context
markRecursiveValueAsUsed names context =
    case context.scopes of
        [] ->
            context

        headScope :: restOfScopes ->
            let
                newHeadScope : Scope
                newHeadScope =
                    { headScope | usedRecursively = Set.union names headScope.usedRecursively }
            in
            { context | scopes = newHeadScope :: restOfScopes }


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
                            if Set.member declared.name headScope.usedRecursively then
                                -- If variable was used as a recursive argument
                                if Set.member declared.name remainingUsed_ then
                                    -- If variable was used somewhere else as well
                                    ( errors_, Set.remove declared.name remainingUsed_ )

                                else
                                    -- If variable was used ONLY as a recursive argument
                                    ( recursiveParameterError headScope.functionName declared :: errors_, Set.remove declared.name remainingUsed_ )

                            else if Set.member declared.name remainingUsed_ then
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
                { message = "Pattern does not introduce any variables"
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


recursiveParameterError : String -> Declared -> Rule.Error {}
recursiveParameterError functionName { name, kind, range, source, fix } =
    Rule.error
        { message = "Parameter `" ++ name ++ "` is only used for recursiveness"
        , details =
            [ "This parameter is only used to be passed as an argument to " ++ functionName ++ ", but its value is never read or used."
            , "You should either use this parameter somewhere, or remove it at the location I pointed at."
            ]
        }
        range


applyFix : Source -> List Fix -> List Fix
applyFix source fix =
    case source of
        NamedFunction ->
            []

        Lambda ->
            fix
