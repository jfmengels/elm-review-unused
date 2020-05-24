module Tests.NoUnused.Patterns exposing (all)

import NoUnused.Patterns exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "You should either use this value somewhere, or remove it at the location I pointed at."
    ]


all : Test
all =
    describe "NoUnused.Patterns"
        [ describe "Function arguments" functionArgumentTests
        ]


functionArgumentTests : List Test
functionArgumentTests =
    [ test "should report unused arguments" <|
        \() ->
            """
module A exposing (..)
foo : Int -> String -> String -> String
foo one two three =
    three
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern `one` is not used"
                        , details = details
                        , under = "one"
                        }
                    , Review.Test.error
                        { message = "Pattern `two` is not used"
                        , details = details
                        , under = "two"
                        }
                    ]
    , test "should not consider values from other modules" <|
        \() ->
            """
module A exposing (..)
foo one =
    Bar.one
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern `one` is not used"
                        , details = details
                        , under = "one"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                    ]
    ]



{- TODO

   Patterns:
     - [-] AllPattern
     - [-] UnitPattern
     - [-] CharPattern Char
     - [-] StringPattern String
     - [-] IntPattern Int
     - [-] HexPattern Int
     - [-] FloatPattern Float
     - [ ] TuplePattern (List (Node Pattern))
     - [ ] RecordPattern (List (Node String))
     - [ ] UnConsPattern (Node Pattern) (Node Pattern)
     - [ ] ListPattern (List (Node Pattern))
     - [x] VarPattern String
     - [ ] NamedPattern QualifiedNameRef (List (Node Pattern))
     - [ ] AsPattern (Node Pattern) (Node String)
     - [ ] ParenthesizedPattern (Node Pattern)

   Sources:
     - [x] Declaration.FunctionDeclaration { declaration.arguments }
     - [ ] Expression.Lambda { args }
     - [ ] Expression.LetFunction { declaration.arguments }
     - [ ] Expression.LetDestructuring pattern _
     - [ ] Expression.Case ( pattern, _ )

-}
