module NoUnused.CustomTypeConstructorArgs exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import String.Extra


{-| Reports arguments of custom type constructors that are never used.

    config =
        [ NoUnused.CustomTypeConstructorArgs.rule
        ]

Custom type constructors can contain data that is never extracted out of the constructor.
This rule will warn arguments that are always pattern matched using a wildcard (`_`).

For package projects, custom types whose constructors are exposed as part of the package API are not reported.

Note that this rule **may report false positives** if you compare custom types with the `==` or `/=` operators
(and never destructure the custom type), like when you do `value == Just 0`, or store them in lists for instance with
[`assoc-list`](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest).
This rule attempts to detect when the custom type is used in comparisons, but it may still result in false positives.


## Fail

    type CustomType
      = CustomType Used Unused

    case customType of
      CustomType value _ -> value


## Success

    type CustomType
      = CustomType Used Unused

    case customType of
      CustomType value maybeUsed -> value


## When not to enable this rule?

If you like giving names to all arguments when pattern matching, then this rule will not find many problems.
This rule will work well when enabled along with [`NoUnused.Patterns`](./NoUnused-Patterns).

Also, if you like comparing custom types in the way described above, you might pass on this rule, or want to be very careful when enabling it.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.CustomTypeConstructorArgs
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.CustomTypeConstructorArgs" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { exposedModules : Set ModuleName
    , constructorsPerModule : Dict ModuleName ModuleConstructors
    , usedArguments : Dict ( ModuleName, ConstructorName ) (Set Int)
    , customTypesNotToReport : Set ( ModuleName, TypeNameS )
    }


type alias ModuleConstructors =
    { moduleKey : Rule.ModuleKey
    , constructors : Dict ConstructorName { nameRange : Range, args : List Range }
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , isModuleExposed : Bool
    , exposed : Exposing
    , customTypeArgs : List ( TypeName, Dict ConstructorName { nameRange : Range, args : List Range } )
    , usedArguments : Dict ( ModuleName, ConstructorName ) (Set Int)
    , customTypesNotToReport : Set ( ModuleName, TypeNameS )
    }


type TypeName
    = TypeName TypeNameS


type alias TypeNameS =
    String


type alias ConstructorName =
    String


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor (\node context -> ( [], moduleDefinitionVisitor node context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationVisitor node context ))
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionVisitor node context ))


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject projectContext =
    case Maybe.map .project maybeProject of
        Just (Elm.Project.Package package) ->
            let
                exposedModules : List Elm.Module.Name
                exposedModules =
                    case package.exposed of
                        Elm.Project.ExposedList list ->
                            list

                        Elm.Project.ExposedDict list ->
                            List.concatMap Tuple.second list

                exposedNames : Set ModuleName
                exposedNames =
                    exposedModules
                        |> List.map (Elm.Module.toString >> String.split ".")
                        |> Set.fromList
            in
            ( [], { projectContext | exposedModules = exposedNames } )

        _ ->
            ( [], projectContext )


initialProjectContext : ProjectContext
initialProjectContext =
    { exposedModules = Set.empty
    , constructorsPerModule = Dict.empty
    , usedArguments = Dict.empty
    , customTypesNotToReport = Set.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable moduleName projectContext ->
            { lookupTable = lookupTable
            , isModuleExposed = Set.member moduleName projectContext.exposedModules
            , exposed = Exposing.Explicit []
            , customTypeArgs = []
            , usedArguments = Dict.empty
            , customTypesNotToReport = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withModuleName


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleName moduleContext ->
            { exposedModules = Set.empty
            , constructorsPerModule =
                Dict.singleton
                    moduleName
                    { moduleKey = moduleKey
                    , constructors = getNonPublicConstructors moduleContext
                    }
            , usedArguments = replaceLocalModuleNameForDict moduleName moduleContext.usedArguments
            , customTypesNotToReport = replaceLocalModuleNameForSet moduleName moduleContext.customTypesNotToReport
            }
        )
        |> Rule.withModuleKey
        |> Rule.withModuleName


replaceLocalModuleNameForSet : ModuleName -> Set ( ModuleName, comparable ) -> Set ( ModuleName, comparable )
replaceLocalModuleNameForSet moduleName set =
    Set.map
        (\(( moduleNameForType, name ) as untouched) ->
            case moduleNameForType of
                [] ->
                    ( moduleName, name )

                _ ->
                    untouched
        )
        set


replaceLocalModuleNameForDict : ModuleName -> Dict ( ModuleName, comparable ) b -> Dict ( ModuleName, comparable ) b
replaceLocalModuleNameForDict moduleName dict =
    Dict.foldl
        (\(( moduleNameForType, name ) as key) value acc ->
            let
                newKey : ( ModuleName, comparable )
                newKey =
                    case moduleNameForType of
                        [] ->
                            ( moduleName, name )

                        _ ->
                            key
            in
            Dict.insert newKey value acc
        )
        Dict.empty
        dict


{-| Get all custom types from the module whose constructors are not part of the public API of the package.
If the module is private or the project is an application, then all open custom types are collected.
-}
getNonPublicConstructors : ModuleContext -> Dict ConstructorName { nameRange : Range, args : List Range }
getNonPublicConstructors moduleContext =
    if moduleContext.isModuleExposed then
        case moduleContext.exposed of
            Exposing.All _ ->
                Dict.empty

            Exposing.Explicit list ->
                let
                    exposedCustomTypes : Set TypeNameS
                    exposedCustomTypes =
                        List.foldl
                            (\(Node _ exposed) acc ->
                                case exposed of
                                    Exposing.TypeExpose { name, open } ->
                                        case open of
                                            Just _ ->
                                                Set.insert name acc

                                            Nothing ->
                                                acc

                                    _ ->
                                        acc
                            )
                            Set.empty
                            list
                in
                List.foldl
                    (\( TypeName typeName, args ) acc ->
                        if Set.member typeName exposedCustomTypes then
                            acc

                        else
                            Dict.union args acc
                    )
                    Dict.empty
                    moduleContext.customTypeArgs

    else
        List.foldl
            (\( _, args ) acc -> Dict.union args acc)
            Dict.empty
            moduleContext.customTypeArgs


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposedModules = previousContext.exposedModules
    , constructorsPerModule =
        Dict.union
            newContext.constructorsPerModule
            previousContext.constructorsPerModule
    , usedArguments =
        Dict.foldl
            (\key newSet acc ->
                case Dict.get key acc of
                    Just existingSet ->
                        Dict.insert key (Set.union newSet existingSet) acc

                    Nothing ->
                        Dict.insert key newSet acc
            )
            previousContext.usedArguments
            newContext.usedArguments
    , customTypesNotToReport = Set.union newContext.customTypesNotToReport previousContext.customTypesNotToReport
    }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ModuleContext
moduleDefinitionVisitor (Node _ node) moduleContext =
    { moduleContext | exposed = Module.exposingList node }


isNotNever : ModuleNameLookupTable -> Node TypeAnnotation -> Bool
isNotNever lookupTable (Node _ node) =
    case node of
        TypeAnnotation.Typed (Node neverRange ( _, "Never" )) [] ->
            ModuleNameLookupTable.moduleNameAt lookupTable neverRange /= Just [ "Basics" ]

        _ ->
            True



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor (Node _ node) context =
    case node of
        Declaration.FunctionDeclaration function ->
            { context
                | usedArguments =
                    registerUsedPatterns
                        (collectUsedPatternsFromFunctionDeclaration context function)
                        context.usedArguments
            }

        Declaration.CustomTypeDeclaration typeDeclaration ->
            let
                customTypeConstructors : Dict ConstructorName { nameRange : Range, args : List Range }
                customTypeConstructors =
                    List.foldl
                        (\(Node _ constructor) acc ->
                            Dict.insert
                                (Node.value constructor.name)
                                { nameRange = Node.range constructor.name
                                , args = createArguments context.lookupTable constructor.arguments
                                }
                                acc
                        )
                        Dict.empty
                        typeDeclaration.constructors
            in
            { context
                | customTypeArgs = ( TypeName (Node.value typeDeclaration.name), customTypeConstructors ) :: context.customTypeArgs
            }

        _ ->
            context


createArguments : ModuleNameLookupTable -> List (Node TypeAnnotation) -> List Range
createArguments lookupTable arguments =
    List.foldr
        (\argument acc ->
            if isNotNever lookupTable argument then
                Node.range argument :: acc

            else
                acc
        )
        []
        arguments


collectUsedPatternsFromFunctionDeclaration : ModuleContext -> Expression.Function -> List ( ( ModuleName, ConstructorName ), Set Int )
collectUsedPatternsFromFunctionDeclaration context { declaration } =
    collectUsedCustomTypeArgs context.lookupTable (Node.value declaration).arguments



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionVisitor (Node _ node) context =
    case node of
        Expression.CaseExpression { cases } ->
            let
                usedArguments : List ( ( ModuleName, String ), Set Int )
                usedArguments =
                    collectUsedCustomTypeArgs context.lookupTable (List.map Tuple.first cases)
            in
            { context | usedArguments = registerUsedPatterns usedArguments context.usedArguments }

        Expression.LetExpression { declarations } ->
            let
                usedArguments : List ( ( ModuleName, String ), Set Int )
                usedArguments =
                    List.concatMap
                        (\(Node _ declaration) ->
                            case declaration of
                                Expression.LetDestructuring pattern _ ->
                                    collectUsedCustomTypeArgs context.lookupTable [ pattern ]

                                Expression.LetFunction function ->
                                    collectUsedPatternsFromFunctionDeclaration context function
                        )
                        declarations
            in
            { context | usedArguments = registerUsedPatterns usedArguments context.usedArguments }

        Expression.LambdaExpression { args } ->
            { context
                | usedArguments =
                    registerUsedPatterns
                        (collectUsedCustomTypeArgs context.lookupTable args)
                        context.usedArguments
            }

        Expression.OperatorApplication operator _ left right ->
            if operator == "==" || operator == "/=" then
                { context | customTypesNotToReport = findCustomTypes context.lookupTable [ left, right ] context.customTypesNotToReport }

            else
                context

        Expression.Application ((Node _ (Expression.PrefixOperator operator)) :: restOfArgs) ->
            if operator == "==" || operator == "/=" then
                { context | customTypesNotToReport = findCustomTypes context.lookupTable restOfArgs context.customTypesNotToReport }

            else
                context

        _ ->
            context


findCustomTypes : ModuleNameLookupTable -> List (Node Expression) -> Set ( ModuleName, TypeNameS ) -> Set ( ModuleName, TypeNameS )
findCustomTypes lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        (Node range node) :: restOfNodes ->
            case node of
                Expression.FunctionOrValue rawModuleName functionName ->
                    if String.Extra.isCapitalized functionName then
                        case ModuleNameLookupTable.moduleNameAt lookupTable range of
                            Just moduleName ->
                                findCustomTypes lookupTable restOfNodes (Set.insert ( moduleName, functionName ) acc)

                            Nothing ->
                                findCustomTypes lookupTable restOfNodes (Set.insert ( rawModuleName, functionName ) acc)

                    else
                        findCustomTypes lookupTable restOfNodes acc

                Expression.TupledExpression expressions ->
                    findCustomTypes lookupTable (expressions ++ restOfNodes) acc

                Expression.ParenthesizedExpression expression ->
                    findCustomTypes lookupTable (expression :: restOfNodes) acc

                Expression.Application (((Node _ (Expression.FunctionOrValue _ functionName)) as first) :: expressions) ->
                    if String.Extra.isCapitalized functionName then
                        findCustomTypes lookupTable (first :: (expressions ++ restOfNodes)) acc

                    else
                        findCustomTypes lookupTable restOfNodes acc

                Expression.OperatorApplication _ _ left right ->
                    findCustomTypes lookupTable (left :: right :: restOfNodes) acc

                Expression.Negation expression ->
                    findCustomTypes lookupTable (expression :: restOfNodes) acc

                Expression.ListExpr expressions ->
                    findCustomTypes lookupTable (expressions ++ restOfNodes) acc

                _ ->
                    findCustomTypes lookupTable restOfNodes acc


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


collectUsedCustomTypeArgs : ModuleNameLookupTable -> List (Node Pattern) -> List ( ( ModuleName, String ), Set Int )
collectUsedCustomTypeArgs lookupTable nodes =
    collectUsedCustomTypeArgsHelp lookupTable nodes []


collectUsedCustomTypeArgsHelp : ModuleNameLookupTable -> List (Node Pattern) -> List ( ( ModuleName, String ), Set Int ) -> List ( ( ModuleName, String ), Set Int )
collectUsedCustomTypeArgsHelp lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        (Node range pattern) :: restOfNodes ->
            case pattern of
                Pattern.NamedPattern { name } args ->
                    let
                        newAcc : List ( ( ModuleName, String ), Set Int )
                        newAcc =
                            case ModuleNameLookupTable.moduleNameAt lookupTable range of
                                Just moduleName ->
                                    ( ( moduleName, name ), computeUsedPositions 0 args Set.empty ) :: acc

                                Nothing ->
                                    acc
                    in
                    collectUsedCustomTypeArgsHelp lookupTable (args ++ restOfNodes) newAcc

                Pattern.TuplePattern patterns ->
                    collectUsedCustomTypeArgsHelp lookupTable (patterns ++ restOfNodes) acc

                Pattern.ListPattern patterns ->
                    collectUsedCustomTypeArgsHelp lookupTable (patterns ++ restOfNodes) acc

                Pattern.UnConsPattern left right ->
                    collectUsedCustomTypeArgsHelp lookupTable (left :: right :: restOfNodes) acc

                Pattern.ParenthesizedPattern subPattern ->
                    collectUsedCustomTypeArgsHelp lookupTable (subPattern :: restOfNodes) acc

                Pattern.AsPattern subPattern _ ->
                    collectUsedCustomTypeArgsHelp lookupTable (subPattern :: restOfNodes) acc

                _ ->
                    collectUsedCustomTypeArgsHelp lookupTable restOfNodes acc


computeUsedPositions : Int -> List (Node Pattern) -> Set Int -> Set Int
computeUsedPositions index arguments acc =
    case arguments of
        [] ->
            acc

        arg :: restOfArgs ->
            let
                newAcc : Set Int
                newAcc =
                    if isWildcard arg then
                        acc

                    else
                        Set.insert index acc
            in
            computeUsedPositions (index + 1) restOfArgs newAcc


isWildcard : Node Pattern -> Bool
isWildcard (Node _ node) =
    case node of
        Pattern.AllPattern ->
            True

        Pattern.ParenthesizedPattern pattern ->
            isWildcard pattern

        _ ->
            False



-- FINAL EVALUATION


finalEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluation context =
    Dict.foldl (finalEvaluationForSingleModule context) [] context.constructorsPerModule


finalEvaluationForSingleModule : ProjectContext -> ModuleName -> ModuleConstructors -> List (Error { useErrorForModule : () }) -> List (Error { useErrorForModule : () })
finalEvaluationForSingleModule context moduleName { moduleKey, constructors } previousErrors =
    Dict.foldl
        (\constructorName { nameRange, args } acc ->
            let
                constructor : ( ModuleName, ConstructorName )
                constructor =
                    ( moduleName, constructorName )
            in
            if Set.member constructor context.customTypesNotToReport then
                acc

            else
                let
                    usedArgumentPositions : Set Int
                    usedArgumentPositions =
                        Dict.get constructor context.usedArguments |> Maybe.withDefault Set.empty
                in
                errorsForUnusedArguments
                    moduleKey
                    usedArgumentPositions
                    0
                    args
                    acc
        )
        previousErrors
        constructors


errorsForUnusedArguments : Rule.ModuleKey -> Set Int -> Int -> List Range -> List (Error anywhere) -> List (Error anywhere)
errorsForUnusedArguments moduleKey usedArgumentPositions index argRanges acc =
    case argRanges of
        [] ->
            acc

        range :: rest ->
            let
                newAcc : List (Error anywhere)
                newAcc =
                    if Set.member index usedArgumentPositions then
                        acc

                    else
                        error moduleKey range :: acc
            in
            errorsForUnusedArguments
                moduleKey
                usedArgumentPositions
                (index + 1)
                rest
                newAcc


error : Rule.ModuleKey -> Range -> Error anywhere
error moduleKey range =
    Rule.errorForModule moduleKey
        { message = "Argument is never extracted and therefore never used."
        , details =
            [ "This argument is never used. You should either use it somewhere, or remove it at the location I pointed at."
            ]
        }
        range
