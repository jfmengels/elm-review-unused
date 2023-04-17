module NoUnused.AliasRecordFieldsTest exposing (all)

import NoUnused.AliasRecordFields exposing (rule)
import Review.Test
import Test exposing (Test)


unusedError : Review.Test.ExpectedError
unusedError =
    Review.Test.error
        { message = "Unused field `unused`"
        , details = details
        , under = "unused"
        }


details : List String
details =
    [ "This field has been declared and may have been assigned to, but is never used. You may have forgotten to use it where needed. Please do so or remove the field." ]


all : Test
all =
    Test.describe "NoUnused.AliasRecordFields"
        [ Test.test "should report an unused field on a type alias" <|
            \() ->
                """module A exposing (a, b)
type alias Record = {foo:Int,bar:Int,unused:Int}
a : Record -> Int
a r1 = r1.foo
b : Record -> Int
b r2 = r2.bar
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unusedError
                            |> Review.Test.atExactly { start = { row = 2, column = 38 }, end = { row = 2, column = 44 } }
                        ]
        , Test.test "should not report a type alias where all fields are used across more than one function" <|
            \() ->
                """module A exposing (a, b)
type alias Record = {foo:Int,bar:Int}
a : Record -> Int
a r1 = r1.foo
b : Record -> Int
b r2 = r2.bar
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , Test.test "should not report a record referenced on another record" <|
            \() ->
                """module A exposing (a, b)
type alias X = {foo:Int,bar:Int}
type alias Y = {x:X}
a : X -> Int
a x = x.foo
b : Y -> Int
b y = y.x.bar
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , Test.test "should not report a record referenced on a custom type" <|
            \() ->
                """module A exposing (a, b)
type alias X = {foo:Int,bar:Int}
type Y = Y X
a : X -> Int
a x = x.foo
b : Y -> Int
b (Y x) = x.bar
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
