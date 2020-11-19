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
        ]
