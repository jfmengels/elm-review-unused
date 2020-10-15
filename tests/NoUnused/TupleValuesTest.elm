module NoUnused.TupleValuesTest exposing (all)

import NoUnused.TupleValues exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "You should either use this value somewhere, or remove it at the location I pointed at."
    ]


all : Test
all =
    describe "NoUnused.TupleValues"
        [ describe "for tuples defined at the top level" topLevelTests
        , describe "for tuples defined in let blocks" letBlockTests
        ]


topLevelTests : List Test
topLevelTests =
    [ test "reports a tuple with one unused values" <|
        \() ->
            """
module A exposing (foo)

twoTuple : (Bool, Int)
twoTuple =
    (False, 1)

foo : Int
foo =
    let
        (_, a) =
            twoTuple
    in
    a
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value in tuple `twoTuple` is never used."
                        , details = details
                        , under = "Bool"
                        }
                    ]
    , test "does not report when tuple values are used" <|
        \() ->
            """
module A exposing (foo)

twoTuple : (Int, Int)
twoTuple =
    (1, 2)

foo : Int
foo =
    let
        (a, b) =
            twoTuple
    in
    a + b
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "reports a tuple with two unused values" <|
        \() ->
            """
module A exposing (foo)

threeTuple : (Bool, Int, Char)
threeTuple =
    (False, 1, 'h')

foo : Int
foo =
    let
        (_, a, _) =
            threeTuple
    in
    a
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value in tuple `threeTuple` is never used."
                        , details = details
                        , under = "Bool"
                        }
                    , Review.Test.error
                        { message = "Value in tuple `threeTuple` is never used."
                        , details = details
                        , under = "Char"
                        }
                    ]
    ]


letBlockTests : List Test
letBlockTests =
    [ test "reports a tuple with one unused values" <|
        \() ->
            """
module A exposing (foo)

foo : Int
foo =
    let
        twoTuple : (Bool, Int)
        twoTuple =
            (False, 1)

        (_, a) =
            twoTuple
    in
    a
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value in tuple `twoTuple` is never used."
                        , details = details
                        , under = "Bool"
                        }
                    ]
    , test "does not report when tuple values are used" <|
        \() ->
            """
module A exposing (foo)

foo : Int
foo =
    let
        twoTuple : (Int, Int)
        twoTuple =
            (1, 2)

        (a, b) =
            twoTuple
    in
    a + b
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "reports a tuple with two unused values" <|
        \() ->
            """
module A exposing (foo)

foo : Int
foo =
    let
        threeTuple : (Bool, Int, Char)
        threeTuple =
            (False, 1, 'h')

        (_, a, _) =
            threeTuple
    in
    a
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value in tuple `threeTuple` is never used."
                        , details = details
                        , under = "Bool"
                        }
                    , Review.Test.error
                        { message = "Value in tuple `threeTuple` is never used."
                        , details = details
                        , under = "Char"
                        }
                    ]
    ]
