module NoUnused.RecordFieldsTest exposing (all)

import NoUnused.RecordFields exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnused.RecordFields"
        [ test "should not report unused variables" <|
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
        , test "should not report if value is used without a field accessor" <|
            \() ->
                """module A exposing (b)
a = {foo=1, unused=2}
b = thing a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
