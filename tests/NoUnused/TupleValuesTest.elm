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
    describe "NoUnused.Patterns"
        [ describe "in Let destructuring" letDestructuringTests
        ]


letDestructuringTests : List Test
letDestructuringTests =
    [ test "reports unused values" <|
        \() ->
            """
module A exposing (foo)

foo : Int
foo =
    let
        tuple : (Bool, Int)
        tuple =
            (False, 1)

        (_, a) =
            tuple
    in
    a
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple value is never used."
                        , details = details
                        , under = "Bool"
                        }
                    ]
    ]
