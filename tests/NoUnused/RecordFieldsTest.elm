module NoUnused.RecordFieldsTest exposing (all)

import NoUnused.RecordFields exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnused.RecordFields"
        [ test "should not report unused variables" <|
            \() ->
                """module A exposing (b)
a = {foo=1, unused=2}
b = let c = {foo=1}
    in 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an unused field" <|
            \() ->
                """module A exposing (b)
a : {foo:Int,unused:Int}
a = {foo=1, unused=2}
b = a.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused field `unused`"
                            , details = [ "REPLACEME" ]
                            , under = "unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 13 }, end = { row = 3, column = 19 } }
                        ]
        , test "should not report if value is used without a field accessor" <|
            \() ->
                """module A exposing (b)
a = {foo=1, unused=2}
b = thing a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report if value is exposed as part of the module (exposing (..))" <|
            \() ->
                """module A exposing (..)
a = {foo=1, unused=2}
b = a.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report if value is exposed as part of the module (exposing explicitly)" <|
            \() ->
                """module A exposing (a, b)
a = {foo=1, unused=2}
b = a.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "TODO should not check when value is a type alias" <|
            \() ->
                """module A exposing (b, TypeAlias)
type alias TypeAlias = {foo:Int,bar:Int}
a : TypeAlias
a = {foo=1, bar=2}
b = a.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "TODO should not report if record is hidden behind a function" <|
            \() ->
                """module A exposing (b)
a argument = {foo=1, unused=2}
b = a.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report unused fields of argument (using record access)" <|
            \() ->
                """module A exposing (a)
a : {foo:Int, unused:Int} -> Int
a arg = arg.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused field `unused`"
                            , details = [ "REPLACEME" ]
                            , under = "unused"
                            }
                        ]
        , test "should report unused fields of argument (with parens)" <|
            \() ->
                """module A exposing (a)
a : {foo:Int, unused:Int} -> Int
a (arg) = arg.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused field `unused`"
                            , details = [ "REPLACEME" ]
                            , under = "unused"
                            }
                        ]
        , test "should report unused fields of argument (with as pattern)" <|
            \() ->
                """module A exposing (a)
a : {foo:Int, unused:Int} -> Int
a (_ as arg) = arg.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused field `unused`"
                            , details = [ "REPLACEME" ]
                            , under = "unused"
                            }
                        ]
        , test "should report unused fields of argument (with as pattern when using the left pattern)" <|
            \() ->
                """module A exposing (a)
a : {foo:Int, bar:Int,unused:Int} -> Int
a ({bar} as arg) = arg.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused field `unused`"
                            , details = [ "REPLACEME" ]
                            , under = "unused"
                            }
                        ]
        , test "should report unused fields of argument when argument is generic" <|
            \() ->
                """module A exposing (a)
a : {var|foo:Int, unused:Int} -> Int
a arg = arg.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused field `unused`"
                            , details = [ "REPLACEME" ]
                            , under = "unused"
                            }
                        ]
        , test "should report unused fields of argument (using destructuring)" <|
            \() ->
                """module A exposing (a)
a : {foo:Int, unused:Int} -> Int
a {foo} = arg.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused field `unused`"
                            , details = [ "REPLACEME" ]
                            , under = "unused"
                            }
                        ]
        , test "should not report when a generic record input type is found again in the output type" <|
            \() ->
                -- NOTE: I was never able to make this test fail, but I could find this kind of error
                -- being reported in real codebases
                """module A exposing (a)
a : { a | thing : Bool } -> { a | thing : Bool }
a value =
    { value | thing = True }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an unused field in a let function" <|
            \() ->
                """module A exposing (a)
a =
  let
    b : {foo:Int,unused:Int}
    b = {foo=1, unused=2}
  in b.foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused field `unused`"
                            , details = [ "REPLACEME" ]
                            , under = "unused"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 17 }, end = { row = 5, column = 23 } }
                        ]
        , test "should report an unused field in a let destructuration" <|
            \() ->
                """module A exposing (a)
a =
  let
    {foo} = {foo=1, unused=2}
  in foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unused field `unused`"
                            , details = [ "REPLACEME" ]
                            , under = "unused"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 17 }, end = { row = 5, column = 23 } }
                        ]
        ]
