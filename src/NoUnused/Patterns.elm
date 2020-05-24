module NoUnused.Patterns exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import NameVisitor
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnused.Patterns" initialContext
        |> Rule.withDeclarationVisitor declarationVisitor
        |> NameVisitor.withValueVisitor valueVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> Rule.Direction -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Declaration.FunctionDeclaration { declaration } ) ->
            ( [], rememberFunctionImplementation declaration context )

        ( Rule.OnExit, Declaration.FunctionDeclaration { declaration } ) ->
            errorsForFunctionImplementation declaration context

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


rememberPatternList : List (Node Pattern) -> Context -> Context
rememberPatternList list context =
    List.foldl rememberPattern context list


rememberPattern : Node Pattern -> Context -> Context
rememberPattern (Node _ pattern) context =
    case pattern of
        Pattern.VarPattern value ->
            rememberValue value context

        _ ->
            let
                _ =
                    Debug.log "pattern" pattern
            in
            context



--- ON EXIT


errorsForFunctionImplementation : Node Expression.FunctionImplementation -> Context -> ( List (Rule.Error {}), Context )
errorsForFunctionImplementation (Node _ { arguments }) context =
    errorsForPatternList arguments context


errorsForPatternList : List (Node Pattern) -> Context -> ( List (Rule.Error {}), Context )
errorsForPatternList list context =
    case list of
        [] ->
            ( [], context )

        pattern :: rest ->
            let
                ( patternErrors, patternContext ) =
                    errorsForPattern pattern context

                ( restErrors, restContext ) =
                    errorsForPatternList rest patternContext
            in
            ( patternErrors ++ restErrors, restContext )


errorsForPattern : Node Pattern -> Context -> ( List (Rule.Error {}), Context )
errorsForPattern (Node range pattern) context =
    case pattern of
        Pattern.VarPattern value ->
            errorsForValue value range context

        _ ->
            ( [], context )



--- CONTEXT


type alias Context =
    Set String


initialContext : Context
initialContext =
    Set.empty


errorsForValue : String -> Range -> Context -> ( List (Rule.Error {}), Context )
errorsForValue value range context =
    if Set.member value context then
        ( [ Rule.errorWithFix
                { message = "Pattern `" ++ value ++ "` is not used"
                , details =
                    [ "You should either use this value somewhere, or remove it at the location I pointed at."
                    ]
                }
                range
                [ Fix.replaceRangeBy range "_" ]
          ]
        , Set.remove value context
        )

    else
        ( [], context )


rememberValue : String -> Context -> Context
rememberValue value context =
    Set.insert value context


useValue : String -> Context -> Context
useValue value context =
    Set.remove value context
