module NoUnused.Dependencies exposing (rule)

{-| Forbid the use of dependencies that are never used in your project.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Package
import Elm.Project exposing (Project)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Version
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid the use of dependencies that are never used in your project.

A dependency is considered unused if none of its modules are imported in the project.

    config =
        [ NoUnused.Dependencies.rule
        ]


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-elm-review-unused/example --rules NoUnused.Dependencies
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.Dependencies" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor importVisitor


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        moduleNameToDependency : Dict String String
        moduleNameToDependency =
            dependencies
                |> Dict.toList
                |> List.concatMap
                    (\( packageName, dependency ) ->
                        List.map (\{ name } -> ( name, packageName )) (Dependency.modules dependency)
                    )
                |> Dict.fromList
    in
    ( []
    , { projectContext
        | dependencies = dependencies
        , moduleNameToDependency = moduleNameToDependency
      }
    )



-- CONTEXT


type alias ProjectContext =
    { moduleNameToDependency : Dict String String
    , dependencies : Dict String Dependency
    , directProjectDependencies : Set String
    , directTestDependencies : Set String
    , importedModuleNames : Set String
    , importedModuleNamesFromTest : Set String
    , usedDependencies : Set String
    , usedDependenciesFromTest : Set String
    , elmJsonKey : Maybe Rule.ElmJsonKey
    }


type alias ModuleContext =
    Set String


initialProjectContext : ProjectContext
initialProjectContext =
    { moduleNameToDependency = Dict.empty
    , dependencies = Dict.empty
    , directProjectDependencies = Set.empty
    , directTestDependencies = Set.empty
    , importedModuleNames = Set.empty
    , importedModuleNamesFromTest = Set.empty
    , usedDependencies = Set.empty
    , usedDependenciesFromTest = Set.empty
    , elmJsonKey = Nothing
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator (\projectContext -> projectContext.importedModuleNames)


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata importedModuleNames ->
            let
                isSourceDir : Bool
                isSourceDir =
                    Rule.isInSourceDirectories metadata
            in
            { moduleNameToDependency = Dict.empty
            , dependencies = Dict.empty
            , directProjectDependencies = Set.empty
            , directTestDependencies = Set.empty
            , usedDependencies = Set.empty
            , usedDependenciesFromTest = Set.empty
            , importedModuleNames =
                if isSourceDir then
                    importedModuleNames

                else
                    Set.empty
            , importedModuleNamesFromTest =
                if isSourceDir then
                    Set.empty

                else
                    importedModuleNames
            , elmJsonKey = Nothing
            }
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { moduleNameToDependency = previousContext.moduleNameToDependency
    , dependencies = previousContext.dependencies
    , directProjectDependencies = previousContext.directProjectDependencies
    , directTestDependencies = previousContext.directTestDependencies
    , importedModuleNames = Set.union newContext.importedModuleNames previousContext.importedModuleNames
    , importedModuleNamesFromTest = Set.union newContext.importedModuleNamesFromTest previousContext.importedModuleNamesFromTest
    , usedDependencies = Set.union newContext.usedDependencies previousContext.usedDependencies
    , usedDependenciesFromTest = Set.union newContext.usedDependenciesFromTest previousContext.usedDependenciesFromTest
    , elmJsonKey = previousContext.elmJsonKey
    }



-- PROJECT VISITORS


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject projectContext =
    case maybeProject of
        Just { elmJsonKey, project } ->
            let
                ( directProjectDependencies, directTestDependencies ) =
                    case project of
                        Elm.Project.Package { deps, testDeps } ->
                            ( deps
                                |> List.map (Tuple.first >> Elm.Package.toString)
                                |> Set.fromList
                            , testDeps
                                |> List.map (Tuple.first >> Elm.Package.toString)
                                |> Set.fromList
                            )

                        Elm.Project.Application { depsDirect, testDepsDirect } ->
                            ( depsDirect
                                |> List.map (Tuple.first >> Elm.Package.toString)
                                |> Set.fromList
                            , testDepsDirect
                                |> List.map (Tuple.first >> Elm.Package.toString)
                                |> Set.fromList
                            )
            in
            ( []
            , { projectContext
                | elmJsonKey = Just elmJsonKey
                , directProjectDependencies = directProjectDependencies
                , directTestDependencies = directTestDependencies
              }
            )

        Nothing ->
            ( [], projectContext )



-- IMPORT VISITOR


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor node importedModuleNames =
    ( []
    , Set.insert (moduleNameForImport node) importedModuleNames
    )


moduleNameForImport : Node Import -> String
moduleNameForImport node =
    node
        |> Node.value
        |> .moduleName
        |> Node.value
        |> String.join "."



-- FINAL EVALUATION


finalEvaluationForProject : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluationForProject projectContext =
    case projectContext.elmJsonKey of
        Just elmJsonKey ->
            let
                testDeps : Set String
                testDeps =
                    Set.foldl
                        (\importedModuleName acc ->
                            case Dict.get importedModuleName projectContext.moduleNameToDependency of
                                Just dep ->
                                    Set.insert dep acc

                                Nothing ->
                                    acc
                        )
                        Set.empty
                        projectContext.importedModuleNamesFromTest

                depsNotUsedInSrc : Set String
                depsNotUsedInSrc =
                    Set.foldl
                        (\importedModuleName acc ->
                            case Dict.get importedModuleName projectContext.moduleNameToDependency of
                                Just dep ->
                                    Set.insert dep acc

                                Nothing ->
                                    acc
                        )
                        Set.empty
                        projectContext.importedModuleNames
                        |> Set.diff projectContext.directProjectDependencies

                depsNotUsedInSrcButUsedInTests : Set String
                depsNotUsedInSrcButUsedInTests =
                    Set.intersect depsNotUsedInSrc testDeps
                        |> Set.remove "elm/core"

                depsNotUsedInSrcErrors : List String
                depsNotUsedInSrcErrors =
                    Set.diff depsNotUsedInSrc depsNotUsedInSrcButUsedInTests
                        |> Set.remove "elm/core"
                        |> Set.toList

                testDepsNotUsedInTests : List String
                testDepsNotUsedInTests =
                    testDeps
                        |> Set.diff projectContext.directTestDependencies
                        |> Set.remove "elm/core"
                        |> Set.toList
            in
            List.map (error elmJsonKey projectContext.dependencies) depsNotUsedInSrcErrors
                ++ List.map (testError elmJsonKey) testDepsNotUsedInTests
                ++ List.map (onlyTestDependencyError elmJsonKey) (Set.toList depsNotUsedInSrcButUsedInTests)

        Nothing ->
            []


error : Rule.ElmJsonKey -> Dict String Dependency -> String -> Error scope
error elmJsonKey dependencies packageNameStr =
    Rule.errorForElmJsonWithFix elmJsonKey
        (\elmJson ->
            { message = "Unused dependency `" ++ packageNameStr ++ "`"
            , details =
                [ "To remove it, I recommend running the following command:"
                , "    elm-json uninstall " ++ packageNameStr
                ]
            , range = findPackageNameInElmJson packageNameStr elmJson
            }
        )
        (\project ->
            case project of
                Elm.Project.Application application ->
                    case find (isPackageWithName packageNameStr) application.depsDirect of
                        Just ( packageName, version ) ->
                            Elm.Project.Application
                                { application
                                    | depsDirect = List.filter (isPackageWithName packageNameStr >> not) application.depsDirect
                                    , depsIndirect =
                                        if isADependencyOfAnotherDependency packageName application.depsDirect dependencies then
                                            ( packageName, version ) :: application.depsIndirect

                                        else
                                            application.depsIndirect
                                }
                                |> Just

                        Nothing ->
                            Nothing

                Elm.Project.Package packageInfo ->
                    Elm.Project.Package
                        { packageInfo
                            | deps = List.filter (isPackageWithName packageNameStr >> not) packageInfo.deps
                        }
                        |> Just
        )


isPackageWithName : String -> ( Elm.Package.Name, a ) -> Bool
isPackageWithName packageName ( packageName_, _ ) =
    packageName == Elm.Package.toString packageName_


thing : Dict String Dependency -> String -> Elm.Project.ApplicationInfo -> Maybe Project
thing dependencies packageNameStr application =
    case find (isPackageWithName packageNameStr) application.depsDirect of
        Just ( packageName, version ) ->
            Elm.Project.Application
                { application
                    | depsDirect = List.filter (isPackageWithName packageNameStr >> not) application.depsDirect
                    , depsIndirect =
                        if isADependencyOfAnotherDependency packageName application.depsDirect dependencies then
                            ( packageName, version ) :: application.depsIndirect

                        else
                            application.depsIndirect
                }
                |> Just

        Nothing ->
            Nothing


isADependencyOfAnotherDependency : Elm.Package.Name -> Elm.Project.Deps a -> Dict String Dependency -> Bool
isADependencyOfAnotherDependency packageName deps dependencies =
    List.any
        (\( depName, _ ) ->
            case
                Dict.get (Elm.Package.toString depName) dependencies
                    |> Maybe.map Dependency.elmJson
            of
                Just (Elm.Project.Package packageInfo) ->
                    List.any (\( depDependencyName, _ ) -> depDependencyName == packageName) packageInfo.deps

                _ ->
                    False
        )
        deps


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ] == Just 6

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


onlyTestDependencyError : Rule.ElmJsonKey -> String -> Error scope
onlyTestDependencyError elmJsonKey packageName =
    Rule.errorForElmJsonWithFix elmJsonKey
        (\elmJson ->
            { message = "`" ++ packageName ++ "` should be moved to test-dependencies"
            , details =
                [ "This package is not used in the source code, but it is used in tests, and should therefore be moved to the test dependencies. To do so, I recommend running the following commands:"
                , "    elm-json uninstall " ++ packageName ++ "\n" ++ "    elm-json install --test " ++ packageName
                ]
            , range = findPackageNameInElmJson packageName elmJson
            }
        )
        (\project ->
            case project of
                Elm.Project.Application application ->
                    case find (isPackageWithName packageName) application.depsDirect of
                        Just packageDep ->
                            Elm.Project.Application
                                { application
                                    | depsDirect = List.filter (isPackageWithName packageName >> not) application.depsDirect
                                    , testDepsDirect = packageDep :: application.testDepsDirect
                                }
                                |> Just

                        Nothing ->
                            Nothing

                Elm.Project.Package packageInfo ->
                    case find (isPackageWithName packageName) packageInfo.deps of
                        Just packageDep ->
                            Elm.Project.Package
                                { packageInfo
                                    | deps = List.filter (isPackageWithName packageName >> not) packageInfo.deps
                                    , testDeps = packageDep :: packageInfo.testDeps
                                }
                                |> Just

                        Nothing ->
                            Nothing
        )


testError : Rule.ElmJsonKey -> String -> Error scope
testError elmJsonKey packageName =
    Rule.errorForElmJsonWithFix elmJsonKey
        (\elmJson ->
            { message = "Unused test dependency `" ++ packageName ++ "`"
            , details =
                [ "To remove it, I recommend running the following command:"
                , "    elm-json uninstall " ++ packageName
                ]
            , range = findPackageNameInElmJson packageName elmJson
            }
        )
        (\project ->
            case project of
                Elm.Project.Application _ ->
                    Nothing

                Elm.Project.Package packageInfo ->
                    Elm.Project.Package
                        { packageInfo | testDeps = List.filter (isPackageWithName packageName >> not) packageInfo.testDeps }
                        |> Just
        )


findPackageNameInElmJson : String -> String -> Range
findPackageNameInElmJson packageName elmJson =
    elmJson
        |> String.lines
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( row, line ) ->
                case String.indexes ("\"" ++ packageName ++ "\"") line of
                    [] ->
                        Nothing

                    column :: _ ->
                        Just
                            { start =
                                { row = row + 1
                                , column = column + 2
                                }
                            , end =
                                { row = row + 1
                                , column = column + String.length packageName + 2
                                }
                            }
            )
        |> List.head
        |> Maybe.withDefault { start = { row = 1, column = 1 }, end = { row = 10000, column = 1 } }
