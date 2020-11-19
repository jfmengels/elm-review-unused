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
        ]
