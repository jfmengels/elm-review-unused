module NoUnused.DependenciesTest exposing (all)

import Elm.Docs
import Elm.Project
import Json.Decode as Decode
import NoUnused.Dependencies exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Test
import Test exposing (Test, describe, test)


createProject : Maybe String -> String -> Project
createProject maybeTestModule rawElmJson =
    Project.new
        |> Project.addElmJson (createElmJson rawElmJson)
        |> Project.addDependency packageWithBar
        |> Project.addDependency packageWithFoo
        |> Project.addDependency packageWithTestBar
        |> Project.addDependency packageWithTestFoo
        |> (case maybeTestModule of
                Just testModule ->
                    Project.addModule { path = "tests/TestModule.elm", source = testModule }

                Nothing ->
                    identity
           )


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = elmJson
            }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


applicationElmJson : String
applicationElmJson =
    """
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            "author/package-with-foo": "1.0.0",
            "author/package-with-bar": "1.0.0"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {
            "author/package-with-test-foo": "1.0.0",
            "author/package-with-test-bar": "1.0.0"
        },
        "indirect": {}
    }
}"""


packageElmJson : String
packageElmJson =
    """
{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0",
        "author/package-with-foo": "1.0.0 <= v < 2.0.0",
        "author/package-with-bar": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
        "author/package-with-test-foo": "1.0.0 <= v < 2.0.0",
        "author/package-with-test-bar": "1.0.0 <= v < 2.0.0"
    }
}"""


packageWithFoo : Dependency
packageWithFoo =
    let
        modules : List Elm.Docs.Module
        modules =
            [ { name = "Foo"
              , comment = ""
              , unions = []
              , aliases = []
              , values = []
              , binops = []
              }
            ]

        elmJson : Elm.Project.Project
        elmJson =
            .project <| createElmJson """
  {
      "type": "package",
      "name": "author/package-with-foo",
      "summary": "Summary",
      "license": "BSD-3-Clause",
      "version": "1.0.0",
      "exposed-modules": [
          "Foo"
      ],
      "elm-version": "0.19.0 <= v < 0.20.0",
      "dependencies": {
          "elm/core": "1.0.0 <= v < 2.0.0"
      },
      "test-dependencies": {}
  }"""
    in
    Dependency.create
        "author/package-with-foo"
        elmJson
        modules


packageWithBar : Dependency
packageWithBar =
    let
        modules : List Elm.Docs.Module
        modules =
            [ { name = "Bar"
              , comment = ""
              , unions = []
              , aliases = []
              , values = []
              , binops = []
              }
            ]

        elmJson : Elm.Project.Project
        elmJson =
            .project <| createElmJson """
{
  "type": "package",
  "name": "author/package-with-bar",
  "summary": "Summary",
  "license": "BSD-3-Clause",
  "version": "1.0.0",
  "exposed-modules": [
      "Bar"
  ],
  "elm-version": "0.19.0 <= v < 0.20.0",
  "dependencies": {
      "elm/core": "1.0.0 <= v < 2.0.0"
  },
  "test-dependencies": {}
}"""
    in
    Dependency.create
        "author/package-with-bar"
        elmJson
        modules


packageWithTestFoo : Dependency
packageWithTestFoo =
    let
        modules : List Elm.Docs.Module
        modules =
            [ { name = "TestFoo"
              , comment = ""
              , unions = []
              , aliases = []
              , values = []
              , binops = []
              }
            ]

        elmJson : Elm.Project.Project
        elmJson =
            .project <| createElmJson """
  {
      "type": "package",
      "name": "author/package-with-test-foo",
      "summary": "Summary",
      "license": "BSD-3-Clause",
      "version": "1.0.0",
      "exposed-modules": [
          "TestFoo"
      ],
      "elm-version": "0.19.0 <= v < 0.20.0",
      "dependencies": {
          "elm/core": "1.0.0 <= v < 2.0.0"
      },
      "test-dependencies": {}
  }"""
    in
    Dependency.create
        "author/package-with-test-foo"
        elmJson
        modules


packageWithTestBar : Dependency
packageWithTestBar =
    let
        modules : List Elm.Docs.Module
        modules =
            [ { name = "TestBar"
              , comment = ""
              , unions = []
              , aliases = []
              , values = []
              , binops = []
              }
            ]

        elmJson : Elm.Project.Project
        elmJson =
            .project <| createElmJson """
{
  "type": "package",
  "name": "author/package-with-test-bar",
  "summary": "Summary",
  "license": "BSD-3-Clause",
  "version": "1.0.0",
  "exposed-modules": [
      "TestBar"
  ],
  "elm-version": "0.19.0 <= v < 0.20.0",
  "dependencies": {
      "elm/core": "1.0.0 <= v < 2.0.0"
  },
  "test-dependencies": {}
}"""
    in
    Dependency.create
        "author/package-with-test-bar"
        elmJson
        modules


all : Test
all =
    describe "NoUnused.Dependencies"
        [ test "should not report anything if there is no `elm.json` file" <|
            \() ->
                """
module A exposing (a)
a = 1
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report unused dependencies for an application when none of their modules are imported" <|
            \() ->
                """
module A exposing (a)
a = 1
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.runWithProjectData (createProject Nothing applicationElmJson) rule
                    |> Review.Test.expectErrorsForElmJson
                        [ Review.Test.error
                            { message = "Unused dependency `author/package-with-bar`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-bar"
                                ]
                            , under = "author/package-with-bar"
                            }
                        , Review.Test.error
                            { message = "Unused dependency `author/package-with-foo`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-foo"
                                ]
                            , under = "author/package-with-foo"
                            }
                        , Review.Test.error
                            { message = "Unused test dependency `author/package-with-test-bar`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-test-bar"
                                ]
                            , under = "author/package-with-test-bar"
                            }
                        , Review.Test.error
                            { message = "Unused test dependency `author/package-with-test-foo`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-test-foo"
                                ]
                            , under = "author/package-with-test-foo"
                            }
                        ]
        , test "should not report dependencies for an application whose modules are imported" <|
            \() ->
                let
                    testModule =
                        """module TestModule exposing (suite)

import TestFoo
import TestBar

suite = 0
"""
                            |> String.replace "\u{000D}" ""
                in
                """
module A exposing (a)
import Foo
import Bar
a = 1
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.runWithProjectData (createProject (Just testModule) applicationElmJson) rule
                    |> Review.Test.expectNoErrors
        , test "should report unused dependencies for a package when none of their modules are imported" <|
            \() ->
                """
module A exposing (a)
a = 1
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.runWithProjectData (createProject Nothing packageElmJson) rule
                    |> Review.Test.expectErrorsForElmJson
                        [ Review.Test.error
                            { message = "Unused dependency `author/package-with-bar`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-bar"
                                ]
                            , under = "author/package-with-bar"
                            }
                            |> Review.Test.whenFixed ("""{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "author/package-with-foo": "1.0.0 <= v < 2.0.0",
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
        "author/package-with-test-bar": "1.0.0 <= v < 2.0.0",
        "author/package-with-test-foo": "1.0.0 <= v < 2.0.0"
    }
}
""" |> String.replace "\u{000D}" "")
                        , Review.Test.error
                            { message = "Unused dependency `author/package-with-foo`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-foo"
                                ]
                            , under = "author/package-with-foo"
                            }
                            |> Review.Test.whenFixed ("""{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "author/package-with-bar": "1.0.0 <= v < 2.0.0",
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
        "author/package-with-test-bar": "1.0.0 <= v < 2.0.0",
        "author/package-with-test-foo": "1.0.0 <= v < 2.0.0"
    }
}
""" |> String.replace "\u{000D}" "")
                        , Review.Test.error
                            { message = "Unused test dependency `author/package-with-test-bar`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-test-bar"
                                ]
                            , under = "author/package-with-test-bar"
                            }
                            |> Review.Test.whenFixed ("""{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "author/package-with-bar": "1.0.0 <= v < 2.0.0",
        "author/package-with-foo": "1.0.0 <= v < 2.0.0",
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
        "author/package-with-test-foo": "1.0.0 <= v < 2.0.0"
    }
}
""" |> String.replace "\u{000D}" "")
                        , Review.Test.error
                            { message = "Unused test dependency `author/package-with-test-foo`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-test-foo"
                                ]
                            , under = "author/package-with-test-foo"
                            }
                            |> Review.Test.whenFixed ("""{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "author/package-with-bar": "1.0.0 <= v < 2.0.0",
        "author/package-with-foo": "1.0.0 <= v < 2.0.0",
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
        "author/package-with-test-bar": "1.0.0 <= v < 2.0.0"
    }
}
""" |> String.replace "\u{000D}" "")
                        ]
        , test "should not report dependencies for a package whose modules are imported" <|
            \() ->
                let
                    testModule =
                        """module TestModule exposing (suite)

import TestFoo
import TestBar

suite = 0
"""
                            |> String.replace "\u{000D}" ""
                in
                """
module A exposing (a)
import Foo
import Bar
a = 1
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.runWithProjectData (createProject (Just testModule) packageElmJson) rule
                    |> Review.Test.expectNoErrors
        , test "should report dependencies that's only used in tests" <|
            \() ->
                let
                    testModule =
                        """module TestModule exposing (suite)

import Foo
import TestFoo
import TestBar

suite = 0
"""
                            |> String.replace "\u{000D}" ""
                in
                """
module A exposing (a)
import Bar
a = 1
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.runWithProjectData (createProject (Just testModule) applicationElmJson) rule
                    |> Review.Test.expectErrorsForElmJson
                        [ Review.Test.error
                            { message = "`author/package-with-foo` should be moved to test-dependencies"
                            , details =
                                [ "This package is not used in the source code, but it is used in tests, and should therefore be moved to the test dependencies. To do so, I recommend running the following commands:"
                                , "    elm-json uninstall author/package-with-foo\n"
                                    ++ "    elm-json install --test author/package-with-foo"
                                ]
                            , under = "author/package-with-foo"
                            }
                        ]
        , test "should report dependencies that's only used in tests and fix it when it's a package elm.json" <|
            \() ->
                let
                    testModule =
                        """module TestModule exposing (suite)

import Foo
import TestFoo
import TestBar

suite = 0
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "author/package-with-bar": "1.0.0 <= v < 2.0.0",
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
        "author/package-with-foo": "1.0.0 <= v < 2.0.0",
        "author/package-with-test-bar": "1.0.0 <= v < 2.0.0",
        "author/package-with-test-foo": "1.0.0 <= v < 2.0.0"
    }
}
"""
                            |> String.replace "\u{000D}" ""
                in
                """
module A exposing (a)
import Bar
a = 1
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.runWithProjectData (createProject (Just testModule) packageElmJson) rule
                    |> Review.Test.expectErrorsForElmJson
                        [ Review.Test.error
                            { message = "`author/package-with-foo` should be moved to test-dependencies"
                            , details =
                                [ "This package is not used in the source code, but it is used in tests, and should therefore be moved to the test dependencies. To do so, I recommend running the following commands:"
                                , "    elm-json uninstall author/package-with-foo\n"
                                    ++ "    elm-json install --test author/package-with-foo"
                                ]
                            , under = "author/package-with-foo"
                            }
                            |> Review.Test.whenFixed expected
                        ]
        ]
