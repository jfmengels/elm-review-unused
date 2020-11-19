module NoUnused.RecordFieldsTest exposing (all)

import NoUnused.RecordFields exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnused.RecordFields"
        [ Test.skip <|
            test "should not report unused variables" <|
                \() ->
                    """module A exposing (..)
a = {}
b = let c = {foo=1}
    in 1
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
        , test "should report an unused field" <|
            \() ->
                """module A exposing (b)
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
                        ]
        , Test.skip <|
            test "should not report if value is used without a field accessor" <|
                \() ->
                    """module A exposing (b)
a = {foo=1, unused=2}
b = thing a
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
