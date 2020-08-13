module NoUnused.CustomTypeConstructorArgsTest exposing (all)

import NoUnused.CustomTypeConstructorArgs exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


message : String
message =
    "REPLACEME"


details : List String
details =
    [ "REPLACEME" ]


all : Test
all =
    describe "NoUnused.CustomTypeConstructorArgs"
        [ test "should report an error when custom type constructor argument is never used" <|
            \() ->
                """module A exposing (..)
type CustomType
  = B B_Data

b = B ()

something =
  case foo of
    B _ -> ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "B_Data"
                            }
                        ]
        , test "should not report an error if custom type constructor argument is used" <|
            \() ->
                """module A exposing (..)
type CustomType
  = B B_Data

b = B ()

something =
  case foo of
    B value -> value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error only for the unused arguments (multiple arguments)" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor SomeData SomeOtherData

b = Constructor ()

something =
  case foo of
    Constructor _ value -> value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "SomeData"
                            }
                        ]
        , test "should not report an error for used arguments in nested patterns (tuple)" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor SomeData

b = Constructor ()

something =
  case foo of
    (_, Constructor value) -> value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
