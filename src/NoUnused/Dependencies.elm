module NoUnused.Dependencies exposing (rule)

{-| Forbid the use of dependencies that are never used in your project.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Package
import Elm.Project exposing (Project)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Vendor.ListExtra


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
            { fromProjectToModule = Rule.initContextCreator fromProjectToModule
            , fromModuleToProject = Rule.initContextCreator fromModuleToProject |> Rule.withMetadata
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
    ( [], { projectContext | moduleNameToDependency = moduleNameToDependency } )



-- CONTEXT


type alias ProjectContext =
    { moduleNameToDependency : Dict String String
    , directProjectDependencies : Set String
    , directTestDependencies : Set String
    , importedModuleNames : Set String
    , importedModuleNamesFromTest : Set String
    , elmJsonKey : Maybe Rule.ElmJsonKey
    }


type alias ModuleContext =
    Set String


initialProjectContext : ProjectContext
initialProjectContext =
    { moduleNameToDependency = Dict.empty
    , directProjectDependencies = Set.empty
    , directTestDependencies = Set.empty
    , importedModuleNames = Set.empty
    , importedModuleNamesFromTest = Set.empty
    , elmJsonKey = Nothing
    }


fromProjectToModule : ProjectContext -> ModuleContext
fromProjectToModule projectContext =
    projectContext.importedModuleNames


fromModuleToProject : Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject metadata importedModuleNames =
    let
        isSourceDir =
            Rule.isInSourceDirectories metadata
    in
    { moduleNameToDependency = Dict.empty
    , directProjectDependencies = Set.empty
    , directTestDependencies = Set.empty
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


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { moduleNameToDependency = previousContext.moduleNameToDependency
    , directProjectDependencies = previousContext.directProjectDependencies
    , directTestDependencies = previousContext.directTestDependencies
    , importedModuleNames = Set.union previousContext.importedModuleNames newContext.importedModuleNames
    , importedModuleNamesFromTest = Set.union previousContext.importedModuleNamesFromTest newContext.importedModuleNamesFromTest
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
                depsNotUsedInSrc : Set String
                depsNotUsedInSrc =
                    projectContext.importedModuleNames
                        |> Set.toList
                        |> List.filterMap (\importedModuleName -> Dict.get importedModuleName projectContext.moduleNameToDependency)
                        |> Set.fromList
                        |> Set.diff projectContext.directProjectDependencies

                depsNotUsedInSrcErrors : List String
                depsNotUsedInSrcErrors =
                    Set.diff depsNotUsedInSrc depsNotUsedInSrcButUsedInTests
                        |> Set.remove "elm/core"
                        |> Set.toList

                testDeps : Set String
                testDeps =
                    projectContext.importedModuleNamesFromTest
                        |> Set.toList
                        |> List.filterMap (\importedModuleName -> Dict.get importedModuleName projectContext.moduleNameToDependency)
                        |> Set.fromList

                depsNotUsedInSrcButUsedInTests : Set String
                depsNotUsedInSrcButUsedInTests =
                    Set.intersect depsNotUsedInSrc testDeps
                        |> Set.remove "elm/core"

                testDepsNotUsedInTests : List String
                testDepsNotUsedInTests =
                    testDeps
                        |> Set.diff projectContext.directTestDependencies
                        |> Set.remove "elm/core"
                        |> Set.toList
            in
            List.map (error elmJsonKey) depsNotUsedInSrcErrors
                ++ List.map (testError elmJsonKey) testDepsNotUsedInTests
                ++ List.map (onlyTestDependencyError elmJsonKey) (Set.toList depsNotUsedInSrcButUsedInTests)

        Nothing ->
            []


error : Rule.ElmJsonKey -> String -> Error scope
error elmJsonKey packageName =
    Rule.errorForElmJsonWithFix elmJsonKey
        (\elmJson ->
            { message = "Unused dependency `" ++ packageName ++ "`"
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
                        { packageInfo
                            | deps =
                                List.filter
                                    (\( packageName_, _ ) -> packageName /= Elm.Package.toString packageName_)
                                    packageInfo.deps
                        }
                        |> Just
        )


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
                Elm.Project.Application _ ->
                    Nothing

                Elm.Project.Package packageInfo ->
                    case
                        Vendor.ListExtra.find
                            (\( packageName_, _ ) -> packageName == Elm.Package.toString packageName_)
                            packageInfo.deps
                    of
                        Just packageDep ->
                            Elm.Project.Package
                                { packageInfo
                                    | deps =
                                        List.filter
                                            (\( packageName_, _ ) -> packageName /= Elm.Package.toString packageName_)
                                            packageInfo.deps
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
                        { packageInfo
                            | testDeps =
                                List.filter
                                    (\( packageName_, _ ) -> packageName /= Elm.Package.toString packageName_)
                                    packageInfo.testDeps
                        }
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
