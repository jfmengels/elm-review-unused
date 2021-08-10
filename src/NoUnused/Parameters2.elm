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
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> NameVisitor.withValueVisitor valueVisitor
        |> Rule.fromModuleRuleSchema



--- CONTEXT


type alias Context =
    { knownParameters : Set String
    , scopes : List Scope
    , scopesToCreate : RangeDict (List FoundPattern)
    }


type alias Scope =
    { declared : List FoundPattern
    , used : Set String
    }


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
    { knownParameters = Set.empty
    , scopes = []
    , scopesToCreate = RangeDict.empty
    }


rememberValue : String -> Context -> Context
rememberValue value context =
    { context | knownParameters = Set.insert value context.knownParameters }


useValue : String -> Context -> Context
useValue value context =
    { context | knownParameters = Set.remove value context.knownParameters }


forgetNode : Node String -> Context -> Context
forgetNode (Node _ value) context =
    { context | knownParameters = Set.remove value context.knownParameters }


declarationEnterVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            ( [], rememberFunctionImplementation declaration { context | scopes = [], scopesToCreate = RangeDict.empty } )

        _ ->
            ( [], context )


declarationExitVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationExitVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            errorsForFunctionImplementation declaration context

        _ ->
            ( [], context )


expressionEnterVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
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


expressionEnterVisitorHelp : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.LambdaExpression { args } ->
            ( [], rememberPatternList args context )

        Expression.LetExpression { declarations } ->
            ( [], rememberLetDeclarationList declarations context )

        _ ->
            ( [], context )


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    case RangeDict.get (Node.range node) context.scopesToCreate of
        Just _ ->
            let
                ( errors, newContext ) =
                    expressionExitVisitorHelp node context
            in
            ( errors, { newContext | scopes = List.drop 1 newContext.scopes } )

        Nothing ->
            expressionExitVisitorHelp node context


expressionExitVisitorHelp : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitorHelp node context =
    case Node.value node of
        Expression.LambdaExpression { args } ->
            errorsForPatternList Lambda args context

        Expression.LetExpression { declarations } ->
            errorsForLetDeclarationList declarations context

        _ ->
            ( [], context )


valueVisitor : Node ( ModuleName, String ) -> Context -> ( List (Rule.Error {}), Context )
valueVisitor (Node _ ( moduleName, value )) context =
    case moduleName of
        [] ->
            ( [], useValue value context )

        _ ->
            ( [], context )



--- ON ENTER


rememberFunctionImplementation : Node Expression.FunctionImplementation -> Context -> Context
rememberFunctionImplementation (Node _ { arguments }) context =
    rememberPatternList arguments context


rememberLetDeclarationList : List (Node Expression.LetDeclaration) -> Context -> Context
rememberLetDeclarationList list context =
    List.foldl rememberLetDeclaration context list


rememberLetDeclaration : Node Expression.LetDeclaration -> Context -> Context
rememberLetDeclaration (Node _ letDeclaration) context =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            rememberLetFunctionImplementation declaration context

        Expression.LetDestructuring _ _ ->
            context


rememberLetFunctionImplementation : Node Expression.FunctionImplementation -> Context -> Context
rememberLetFunctionImplementation (Node _ { arguments }) context =
    rememberPatternList arguments context


rememberPatternList : List (Node Pattern) -> Context -> Context
rememberPatternList list context =
    List.foldl rememberPattern context list


rememberPattern : Node Pattern -> Context -> Context
rememberPattern (Node _ pattern) context =
    case pattern of
        Pattern.AllPattern ->
            context

        Pattern.VarPattern value ->
            rememberValue value context

        Pattern.TuplePattern patterns ->
            rememberPatternList patterns context

        Pattern.RecordPattern values ->
            rememberRecordPattern values context

        Pattern.UnConsPattern first second ->
            rememberPatternList [ first, second ] context

        Pattern.ListPattern patterns ->
            rememberPatternList patterns context

        Pattern.NamedPattern _ patterns ->
            rememberPatternList patterns context

        Pattern.AsPattern inner name ->
            rememberPattern inner (rememberValue (Node.value name) context)

        Pattern.ParenthesizedPattern inner ->
            rememberPattern inner context

        _ ->
            context


rememberRecordPattern : List (Node String) -> Context -> Context
rememberRecordPattern list context =
    List.foldl (Node.value >> rememberValue) context list



--- ON EXIT


singularDetails : List String
singularDetails =
    [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]


pluralDetails : List String
pluralDetails =
    [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]


removeDetails : List String
removeDetails =
    [ "You should remove it at the location I pointed at." ]


andThen :
    (value -> Context -> ( List (Rule.Error {}), Context ))
    -> value
    -> ( List (Rule.Error {}), Context )
    -> ( List (Rule.Error {}), Context )
andThen function value ( errors, context ) =
    let
        ( newErrors, newContext ) =
            function value context
    in
    ( newErrors ++ errors, newContext )


errorsForFunctionImplementation : Node Expression.FunctionImplementation -> Context -> ( List (Rule.Error {}), Context )
errorsForFunctionImplementation (Node _ { arguments }) context =
    errorsForPatternList Function arguments context


errorsForLetDeclarationList : List (Node Expression.LetDeclaration) -> Context -> ( List (Rule.Error {}), Context )
errorsForLetDeclarationList list context =
    List.foldl (andThen errorsForLetDeclaration) ( [], context ) list


errorsForLetDeclaration : Node Expression.LetDeclaration -> Context -> ( List (Rule.Error {}), Context )
errorsForLetDeclaration (Node _ letDeclaration) context =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            errorsForLetFunctionImplementation declaration context

        Expression.LetDestructuring _ _ ->
            ( [], context )


errorsForLetFunctionImplementation : Node Expression.FunctionImplementation -> Context -> ( List (Rule.Error {}), Context )
errorsForLetFunctionImplementation (Node _ { arguments }) context =
    errorsForPatternList Function arguments context


type PatternUse
    = Lambda
    | Function


errorsForPatternList : PatternUse -> List (Node Pattern) -> Context -> ( List (Rule.Error {}), Context )
errorsForPatternList use list context =
    List.foldl (andThen (errorsForPattern use)) ( [], context ) list


errorsForPattern : PatternUse -> Node Pattern -> Context -> ( List (Rule.Error {}), Context )
errorsForPattern use (Node range pattern) context =
    case pattern of
        Pattern.AllPattern ->
            ( [], context )

        Pattern.VarPattern value ->
            errorsForValue use value range context

        Pattern.RecordPattern values ->
            errorsForRecordValueList use range values context

        Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
            errorsForUselessTuple use range context

        Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
            errorsForUselessTuple use range context

        Pattern.TuplePattern patterns ->
            errorsForPatternList use patterns context

        Pattern.UnConsPattern first second ->
            errorsForPatternList use [ first, second ] context

        Pattern.ListPattern patterns ->
            errorsForPatternList use patterns context

        Pattern.NamedPattern _ patterns ->
            if List.all isAllPattern patterns then
                errorsForUselessNamePattern use range context

            else
                errorsForPatternList use patterns context

        Pattern.AsPattern inner name ->
            context
                |> errorsForAsPattern use range inner name
                |> andThen (errorsForPattern use) inner

        Pattern.ParenthesizedPattern inner ->
            errorsForPattern use inner context

        _ ->
            ( [], context )


errorsForUselessNamePattern : PatternUse -> Range -> Context -> ( List (Rule.Error {}), Context )
errorsForUselessNamePattern use range context =
    let
        fix : List Fix
        fix =
            case use of
                Lambda ->
                    [ Fix.replaceRangeBy range "_" ]

                Function ->
                    []
    in
    ( [ Rule.errorWithFix
            { message = "Named pattern is not needed"
            , details = removeDetails
            }
            range
            fix
      ]
    , context
    )


errorsForUselessTuple : PatternUse -> Range -> Context -> ( List (Rule.Error {}), Context )
errorsForUselessTuple use range context =
    let
        fix : List Fix
        fix =
            case use of
                Lambda ->
                    [ Fix.replaceRangeBy range "_" ]

                Function ->
                    []
    in
    ( [ Rule.errorWithFix
            { message = "Tuple pattern is not needed"
            , details = removeDetails
            }
            range
            fix
      ]
    , context
    )


errorsForRecordValueList : PatternUse -> Range -> List (Node String) -> Context -> ( List (Rule.Error {}), Context )
errorsForRecordValueList use recordRange list context =
    if List.isEmpty list then
        ( [ Rule.errorWithFix
                { message = "Record pattern is not needed"
                , details = [ "This pattern is redundant and should be replaced with '_'." ]
                }
                recordRange
                [ Fix.replaceRangeBy recordRange "_" ]
          ]
        , context
        )

    else
        let
            ( unused, used ) =
                List.partition (isNodeInContext context) list
        in
        case unused of
            [] ->
                ( [], context )

            firstNode :: restNodes ->
                let
                    first : String
                    first =
                        Node.value firstNode

                    rest : List String
                    rest =
                        List.map Node.value restNodes

                    errorRange : Range
                    errorRange =
                        Range.combine (List.map Node.range unused)

                    fix : List Fix
                    fix =
                        case ( use, used ) of
                            ( Lambda, [] ) ->
                                [ Fix.replaceRangeBy recordRange "_" ]

                            ( Lambda, _ ) ->
                                [ Node Range.emptyRange (Pattern.RecordPattern used)
                                    |> Writer.writePattern
                                    |> Writer.write
                                    |> Fix.replaceRangeBy recordRange
                                ]

                            ( Function, _ ) ->
                                []
                in
                ( [ Rule.errorWithFix
                        { message = listToMessage first rest
                        , details = listToDetails first rest
                        }
                        errorRange
                        fix
                  ]
                , List.foldl forgetNode context unused
                )


isNodeInContext : Context -> Node String -> Bool
isNodeInContext context (Node _ value) =
    Set.member value context.knownParameters


listToMessage : String -> List String -> String
listToMessage first rest =
    case List.reverse rest of
        [] ->
            "Parameter `" ++ first ++ "` is not used."

        last :: middle ->
            "Parameters `" ++ String.join "`, `" (first :: middle) ++ "` and `" ++ last ++ "` are not used."


listToDetails : String -> List String -> List String
listToDetails _ rest =
    case rest of
        [] ->
            singularDetails

        _ ->
            pluralDetails


errorsForAsPattern : PatternUse -> Range -> Node Pattern -> Node String -> Context -> ( List (Rule.Error {}), Context )
errorsForAsPattern use patternRange inner (Node range name) context =
    if Set.member name context.knownParameters then
        let
            fix : List Fix
            fix =
                case use of
                    Lambda ->
                        [ inner
                            |> Writer.writePattern
                            |> Writer.write
                            |> Fix.replaceRangeBy patternRange
                        ]

                    Function ->
                        []
        in
        ( [ Rule.errorWithFix
                { message = "Pattern alias `" ++ name ++ "` is not used."
                , details = singularDetails
                }
                range
                fix
          ]
        , useValue name context
        )

    else if isAllPattern inner then
        ( [ Rule.errorWithFix
                { message = "Pattern `_` is not needed"
                , details = removeDetails
                }
                (Node.range inner)
                [ Fix.replaceRangeBy patternRange name ]
          ]
        , useValue name context
        )

    else
        ( [], context )


isAllPattern : Node Pattern -> Bool
isAllPattern (Node _ pattern) =
    case pattern of
        Pattern.AllPattern ->
            True

        _ ->
            False


errorsForValue : PatternUse -> String -> Range -> Context -> ( List (Rule.Error {}), Context )
errorsForValue use value range context =
    if Set.member value context.knownParameters then
        let
            fix : List Fix
            fix =
                case use of
                    Lambda ->
                        [ Fix.replaceRangeBy range "_" ]

                    Function ->
                        []
        in
        ( [ Rule.errorWithFix
                { message = "Parameter `" ++ value ++ "` is not used."
                , details = singularDetails
                }
                range
                fix
          ]
        , useValue value context
        )

    else
        ( [], context )
