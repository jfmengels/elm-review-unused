module NoUnused.CustomTypeConstructorArgs exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Scope
import Set exposing (Set)


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
    Rule.newProjectRuleSchema "NoUnused.CustomTypeConstructorArgs" initialProjectContext
        |> Scope.addProjectVisitors
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { scope : Scope.ProjectContext
    , customTypeArgs : Dict ModuleName { moduleKey : Rule.ModuleKey, args : Dict String (List Range) }
    , usedArguments : Dict ( ModuleName, String ) (Set Int)
    }


type alias ModuleContext =
    { scope : Scope.ModuleContext
    , customTypeArgs : Dict String (List Range)
    , usedArguments : Dict ( ModuleName, String ) (Set Int)
    }


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor


initialProjectContext : ProjectContext
initialProjectContext =
    { scope = Scope.initialProjectContext
    , customTypeArgs = Dict.empty
    , usedArguments = Dict.empty
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ _ projectContext =
    { scope = Scope.fromProjectToModule projectContext.scope
    , customTypeArgs = Dict.empty
    , usedArguments = Dict.empty
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleName moduleContext =
    { scope = Scope.fromModuleToProject moduleName moduleContext.scope
    , customTypeArgs =
        Dict.singleton
            (Node.value moduleName)
            { moduleKey = moduleKey, args = moduleContext.customTypeArgs }
    , usedArguments =
        Dict.foldl
            (\( moduleNameForType, name ) value dict ->
                case moduleNameForType of
                    [] ->
                        Dict.insert ( Node.value moduleName, name ) value dict

                    _ ->
                        Dict.insert ( moduleNameForType, name ) value dict
            )
            Dict.empty
            moduleContext.usedArguments
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { scope = Scope.foldProjectContexts newContext.scope previousContext.scope
    , customTypeArgs =
        Dict.union
            newContext.customTypeArgs
            previousContext.customTypeArgs
    , usedArguments =
        Dict.merge
            Dict.insert
            (\key newSet prevSet dict -> Dict.insert key (Set.union newSet prevSet) dict)
            Dict.insert
            newContext.usedArguments
            previousContext.usedArguments
            Dict.empty
    }



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
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



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( []
            , { context
                | usedArguments =
                    registerUsedPatterns
                        (collectUsedPatternsFromFunctionDeclaration context function)
                        context.usedArguments
              }
            )

        _ ->
            ( [], context )


collectUsedPatternsFromFunctionDeclaration : ModuleContext -> Expression.Function -> List ( ( ModuleName, String ), Set Int )
collectUsedPatternsFromFunctionDeclaration context { declaration } =
    (Node.value declaration).arguments
        |> List.concatMap (collectUsedCustomTypeArgs context.scope)



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            let
                usedArguments : List ( ( ModuleName, String ), Set Int )
                usedArguments =
                    cases
                        |> List.concatMap (Tuple.first >> collectUsedCustomTypeArgs context.scope)
            in
            ( [], { context | usedArguments = registerUsedPatterns usedArguments context.usedArguments } )

        Expression.LetExpression { declarations } ->
            let
                usedArguments : List ( ( ModuleName, String ), Set Int )
                usedArguments =
                    List.concatMap
                        (\declaration ->
                            case Node.value declaration of
                                Expression.LetDestructuring pattern _ ->
                                    collectUsedCustomTypeArgs context.scope pattern

                                Expression.LetFunction function ->
                                    collectUsedPatternsFromFunctionDeclaration context function
                        )
                        declarations
            in
            ( [], { context | usedArguments = registerUsedPatterns usedArguments context.usedArguments } )

        _ ->
            ( [], context )


registerUsedPatterns : List ( ( ModuleName, String ), Set Int ) -> Dict ( ModuleName, String ) (Set Int) -> Dict ( ModuleName, String ) (Set Int)
registerUsedPatterns newUsedArguments previouslyUsedArguments =
    List.foldl
        (\( key, usedPositions ) acc ->
            let
                previouslyUsedPositions : Set Int
                previouslyUsedPositions =
                    Dict.get key acc
                        |> Maybe.withDefault Set.empty
            in
            Dict.insert key (Set.union previouslyUsedPositions usedPositions) acc
        )
        previouslyUsedArguments
        newUsedArguments


collectUsedCustomTypeArgs : Scope.ModuleContext -> Node Pattern -> List ( ( ModuleName, String ), Set Int )
collectUsedCustomTypeArgs scope (Node _ pattern) =
    case pattern of
        Pattern.NamedPattern { moduleName, name } args ->
            let
                usedPositions : Set Int
                usedPositions =
                    args
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, subPattern ) -> not <| isWildcard subPattern)
                        |> List.map Tuple.first
                        |> Set.fromList
            in
            [ ( ( Scope.moduleNameForValue scope name moduleName, name ), usedPositions ) ]
                ++ List.concatMap (collectUsedCustomTypeArgs scope) args

        Pattern.TuplePattern patterns ->
            List.concatMap (collectUsedCustomTypeArgs scope) patterns

        Pattern.ListPattern patterns ->
            List.concatMap (collectUsedCustomTypeArgs scope) patterns

        Pattern.UnConsPattern left right ->
            List.concatMap (collectUsedCustomTypeArgs scope) [ left, right ]

        Pattern.ParenthesizedPattern subPattern ->
            collectUsedCustomTypeArgs scope subPattern

        Pattern.AsPattern subPattern _ ->
            collectUsedCustomTypeArgs scope subPattern

        _ ->
            []


isWildcard : Node Pattern -> Bool
isWildcard node =
    case Node.value node of
        Pattern.AllPattern ->
            True

        Pattern.ParenthesizedPattern pattern ->
            isWildcard pattern

        _ ->
            False



-- FINAL EVALUATION


finalEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluation context =
    context.customTypeArgs
        |> Dict.toList
        |> List.concatMap
            (\( moduleName, { moduleKey, args } ) ->
                args
                    |> Dict.toList
                    |> List.concatMap
                        (\( name, ranges ) ->
                            case Dict.get ( moduleName, name ) context.usedArguments of
                                Just usedArgumentPositions ->
                                    ranges
                                        |> List.indexedMap Tuple.pair
                                        |> List.filter (\( index, _ ) -> not <| Set.member index usedArgumentPositions)
                                        |> List.map (Tuple.second >> error moduleKey)

                                Nothing ->
                                    List.map (error moduleKey) ranges
                        )
            )


error : Rule.ModuleKey -> Range -> Error anywhere
error moduleKey range =
    Rule.errorForModule moduleKey
        { message = "REPLACEME"
        , details = [ "REPLACEME" ]
        }
        range
