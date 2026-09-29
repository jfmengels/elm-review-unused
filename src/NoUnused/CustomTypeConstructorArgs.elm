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
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
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
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
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
    , dependencyModules : Set ModuleName
    , constructorsPerModule : Dict ModuleName ModuleConstructors
    , usedArguments : Dict ( ModuleName, ConstructorName ) (Set Int)
    , unusedArgumentsInPatterns :
        Dict
            ( Int, ModuleName, ConstructorName )
            {- `Just [ ... ]` is the list of unused arguments.
               `Just Nothing` means we have found at least one location where it's used, and we don't want to report it.
            -}
            (Maybe (List { moduleKey : Rule.ModuleKey, args : List Range }))
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
    , dependencyModules : Set ModuleName
    , customTypeArgs : List ( TypeName, Dict ConstructorName { nameRange : Range, args : List Range } )
    , usedArguments : Dict ( ModuleName, ConstructorName ) (Set Int)
    , unusedArgumentsInPatterns :
        Dict
            ( Int, ModuleName, ConstructorName )
            {- `Just [ ... ]` is the list of unused arguments.
               `Just Nothing` means we have found at least one location where it's used, and we don't want to report it.
            -}
            (Maybe (List Range))
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


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        dependencyModules : Set ModuleName
        dependencyModules =
            Dict.foldl
                (\_ dep set ->
                    List.foldl (\{ name } set_ -> Set.insert (String.split "." name) set_)
                        set
                        (Dependency.modules dep)
                )
                Set.empty
                dependencies
    in
    ( [], { projectContext | dependencyModules = dependencyModules } )


initialProjectContext : ProjectContext
initialProjectContext =
    { exposedModules = Set.empty
    , dependencyModules = Set.empty
    , constructorsPerModule = Dict.empty
    , usedArguments = Dict.empty
    , unusedArgumentsInPatterns = Dict.empty
    , customTypesNotToReport = Set.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable moduleName projectContext ->
            { lookupTable = lookupTable
            , isModuleExposed = Set.member moduleName projectContext.exposedModules
            , dependencyModules = projectContext.dependencyModules
            , exposed = Exposing.Explicit []
            , customTypeArgs = []
            , usedArguments = Dict.empty
            , unusedArgumentsInPatterns = Dict.empty
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
            , dependencyModules = Set.empty
            , constructorsPerModule =
                Dict.singleton
                    moduleName
                    { moduleKey = moduleKey
                    , constructors = getNonPublicConstructors moduleContext
                    }
            , usedArguments = replaceLocalModuleNameForDict moduleName moduleContext.usedArguments
            , unusedArgumentsInPatterns = Dict.map (\_ args -> Maybe.map (\args_ -> [ { moduleKey = moduleKey, args = args_ } ]) args) moduleContext.unusedArgumentsInPatterns
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
    , dependencyModules = previousContext.dependencyModules
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
    , unusedArgumentsInPatterns =
        Dict.foldl
            (\key value dict ->
                case Dict.get key dict of
                    Just Nothing ->
                        dict

                    Just (Just list) ->
                        Dict.insert key (Maybe.map (\v -> v ++ list) value) dict

                    Nothing ->
                        Dict.insert key value dict
            )
            newContext.unusedArgumentsInPatterns
            previousContext.unusedArgumentsInPatterns
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
            { context
                | usedArguments = registerUsedPatterns usedArguments context.usedArguments
                , unusedArgumentsInPatterns = collectCustomTypeArgsInPatterns context (List.map Tuple.first cases) context.unusedArgumentsInPatterns
            }

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
                { context | customTypesNotToReport = findCustomTypes context [ left, right ] context.customTypesNotToReport }

            else
                context

        Expression.Application ((Node _ (Expression.PrefixOperator operator)) :: restOfArgs) ->
            if operator == "==" || operator == "/=" then
                { context | customTypesNotToReport = findCustomTypes context restOfArgs context.customTypesNotToReport }

            else
                context

        _ ->
            context


findCustomTypes : ModuleContext -> List (Node Expression) -> Set ( ModuleName, TypeNameS ) -> Set ( ModuleName, TypeNameS )
findCustomTypes context nodes acc =
    case nodes of
        [] ->
            acc

        (Node range node) :: restOfNodes ->
            case node of
                Expression.FunctionOrValue rawModuleName functionName ->
                    if String.Extra.isCapitalized functionName then
                        let
                            moduleName : ModuleName
                            moduleName =
                                ModuleNameLookupTable.moduleNameAt context.lookupTable range
                                    |> Maybe.withDefault rawModuleName
                        in
                        if Set.member moduleName context.dependencyModules then
                            findCustomTypes context restOfNodes acc

                        else
                            findCustomTypes context restOfNodes (Set.insert ( moduleName, functionName ) acc)

                    else
                        findCustomTypes context restOfNodes acc

                Expression.TupledExpression expressions ->
                    findCustomTypes context (expressions ++ restOfNodes) acc

                Expression.ParenthesizedExpression expression ->
                    findCustomTypes context (expression :: restOfNodes) acc

                Expression.Application (((Node _ (Expression.FunctionOrValue _ functionName)) as first) :: expressions) ->
                    if String.Extra.isCapitalized functionName then
                        findCustomTypes context (first :: (expressions ++ restOfNodes)) acc

                    else
                        findCustomTypes context restOfNodes acc

                Expression.OperatorApplication _ _ left right ->
                    findCustomTypes context (left :: right :: restOfNodes) acc

                Expression.Negation expression ->
                    findCustomTypes context (expression :: restOfNodes) acc

                Expression.ListExpr expressions ->
                    findCustomTypes context (expressions ++ restOfNodes) acc

                _ ->
                    findCustomTypes context restOfNodes acc


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


collectCustomTypeArgsInPatterns :
    ModuleContext
    -> List (Node Pattern)
    -> Dict ( Int, ModuleName, ConstructorName ) (Maybe (List Range))
    -> Dict ( Int, ModuleName, ConstructorName ) (Maybe (List Range))
collectCustomTypeArgsInPatterns context nodes acc =
    case nodes of
        [] ->
            acc

        (Node range pattern) :: restOfNodes ->
            case pattern of
                Pattern.NamedPattern ref args ->
                    let
                        newAcc : Dict ( Int, ModuleName, ConstructorName ) (Maybe (List Range))
                        newAcc =
                            case ModuleNameLookupTable.fullModuleNameAt context.lookupTable range of
                                Just moduleName ->
                                    if Set.member moduleName context.dependencyModules then
                                        acc

                                    else
                                        let
                                            endPositionOfName : Location
                                            endPositionOfName =
                                                { row = range.end.row
                                                , column = range.start.column + String.length (String.join "." (ref.name :: ref.moduleName))
                                                }
                                        in
                                        getUnusedConstructorFields moduleName ref.name 0 args endPositionOfName acc

                                Nothing ->
                                    acc
                    in
                    collectCustomTypeArgsInPatterns context (args ++ restOfNodes) newAcc

                Pattern.TuplePattern patterns ->
                    collectCustomTypeArgsInPatterns context (patterns ++ restOfNodes) acc

                Pattern.ListPattern patterns ->
                    collectCustomTypeArgsInPatterns context (patterns ++ restOfNodes) acc

                Pattern.UnConsPattern left right ->
                    collectCustomTypeArgsInPatterns context (left :: right :: restOfNodes) acc

                Pattern.ParenthesizedPattern subPattern ->
                    collectCustomTypeArgsInPatterns context (subPattern :: restOfNodes) acc

                Pattern.AsPattern subPattern _ ->
                    collectCustomTypeArgsInPatterns context (subPattern :: restOfNodes) acc

                _ ->
                    collectCustomTypeArgsInPatterns context restOfNodes acc


getUnusedConstructorFields : ModuleName -> ConstructorName -> Int -> List (Node Pattern) -> Location -> Dict ( Int, ModuleName, ConstructorName ) (Maybe (List Range)) -> Dict ( Int, ModuleName, ConstructorName ) (Maybe (List Range))
getUnusedConstructorFields moduleName constructorName index arguments previousEnd acc =
    case arguments of
        [] ->
            acc

        arg :: restOfArgs ->
            let
                key : ( Int, ModuleName, ConstructorName )
                key =
                    ( index, moduleName, constructorName )

                newAcc : Dict ( Int, ModuleName, ConstructorName ) (Maybe (List Range))
                newAcc =
                    case Dict.get key acc of
                        Just Nothing ->
                            -- We have previously found pattern matches for this constructor field
                            -- and some of them were *not* unused. We will continue to not report this field.
                            acc

                        Just (Just list) ->
                            addWildcardPosition key previousEnd arg list acc

                        Nothing ->
                            addWildcardPosition key previousEnd arg [] acc
            in
            getUnusedConstructorFields
                moduleName
                constructorName
                (index + 1)
                restOfArgs
                (Node.range arg).end
                newAcc


addWildcardPosition :
    ( Int, ModuleName, ConstructorName )
    -> Location
    -> Node Pattern
    -> List Range
    -> Dict ( Int, ModuleName, ConstructorName ) (Maybe (List Range))
    -> Dict ( Int, ModuleName, ConstructorName ) (Maybe (List Range))
addWildcardPosition key previousEnd arg list acc =
    if isWildcard arg then
        Dict.insert key (Just ({ start = previousEnd, end = (Node.range arg).end } :: list)) acc

    else
        -- This constructor field is *not* unused, we therefore insert `Nothing` to disable the rule reporting it.
        Dict.insert key Nothing acc


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
                key : ( ModuleName, ConstructorName )
                key =
                    ( moduleName, constructorName )
            in
            if Set.member key context.customTypesNotToReport then
                acc

            else
                let
                    usedArgumentPositions : Set Int
                    usedArgumentPositions =
                        Dict.get key context.usedArguments |> Maybe.withDefault Set.empty
                in
                errorsForUnusedArguments
                    moduleKey
                    usedArgumentPositions
                    nameRange
                    0
                    args
                    acc
        )
        previousErrors
        constructors


errorsForUnusedArguments : Rule.ModuleKey -> Set Int -> Range -> Int -> List Range -> List (Error anywhere) -> List (Error anywhere)
errorsForUnusedArguments moduleKey usedArgumentPositions previousRange index argRanges acc =
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
                        let
                            fixes : List Rule.FixV2
                            fixes =
                                [ Rule.editModule
                                    moduleKey
                                    [ Fix.removeRange { start = previousRange.end, end = range.end }
                                    ]
                                ]
                        in
                        error moduleKey range fixes :: acc
            in
            errorsForUnusedArguments
                moduleKey
                usedArgumentPositions
                range
                (index + 1)
                rest
                newAcc


error : Rule.ModuleKey -> Range -> List Rule.FixV2 -> Error anywhere
error moduleKey range fixes =
    Rule.errorForModule moduleKey
        { message = "Argument is never extracted and therefore never used."
        , details =
            [ "This argument is never used. You should either use it somewhere, or remove it at the location I pointed at."
            ]
        }
        range
        |> Rule.withFixesV2 fixes
