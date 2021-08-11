module NoUnused.ParametersTests2 exposing (all)

import NoUnused.Parameters2 exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]


all : Test
all =
    describe "NoUnused.Parameters"
        [ describe "in Function arguments" functionArgumentTests
        , describe "in Lambda arguments" lambdaArgumentTests
        , describe "in Let Functions" letFunctionTests

        --- in lambda
        , Test.skip <| describe "with as pattern in lambdas" lambdaAsPatternTests
        , describe "with named pattern in lambdas" lambdaNamedPatternTests
        , Test.skip <| describe "with record pattern in lambdas" lambdaRecordPatternTests
        , Test.skip <| describe "with tuple pattern in lambdas" lambdaTuplePatternTests

        --- in function
        , describe "with as pattern in functions" functionAsPatternTests
        , describe "with named pattern in functions" functionNamedPatternTests
        , describe "with record pattern in functions" functionRecordPatternTests
        , describe "with tuple pattern in functions" functionTuplePatternTests
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
                        { message = "Parameter `one` is not used."
                        , details = details
                        , under = "one"
                        }
                    , Review.Test.error
                        { message = "Parameter `two` is not used."
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
                        { message = "Parameter `one` is not used."
                        , details = details
                        , under = "one"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                    ]
    , test "should not report used parameters (value reference)" <|
        \() ->
            """
module A exposing (..)
foo one =
    one
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report used parameters (record update reference)" <|
        \() ->
            """
module A exposing (..)
foo one =
    { one | a = 1 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


lambdaArgumentTests : List Test
lambdaArgumentTests =
    [ test "should report unused arguments" <|
        \() ->
            """
module A exposing (..)
foo =
    List.map (\\value -> Nothing) list
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `value` is not used."
                        , details = details
                        , under = "value"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    List.map (\\_ -> Nothing) list
"""
                    ]
    ]


letFunctionTests : List Test
letFunctionTests =
    [ test "should report unused arguments" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        one oneValue =
            1
        two twoValue =
            2
    in
    one two 3
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `oneValue` is not used."
                        , details = details
                        , under = "oneValue"
                        }
                    , Review.Test.error
                        { message = "Parameter `twoValue` is not used."
                        , details = details
                        , under = "twoValue"
                        }
                    ]
    , test "should report unused even if others with the same name are used in siblings" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        one oneValue =
            oneValue
        two oneValue =
            1
    in
    one two 3
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `oneValue` is not used."
                        , details = details
                        , under = "oneValue"
                        }
                        |> Review.Test.atExactly { start = { row = 7, column = 13 }, end = { row = 7, column = 21 } }
                    ]
    , test "should not report unused let functions" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        value =
            something 5
    in
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]



--- LAMBDA PATTERN TESTS ------------------------


lambdaAsPatternTests : List Test
lambdaAsPatternTests =
    [ test "should report unused pattern aliases" <|
        \() ->
            """
module A exposing (..)
foo =
    \\({ bish, bash } as bosh) ->
        ( bish, bash )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\({bish, bash}) ->
        ( bish, bash )
"""
                    ]
    , test "should report unused patterns in an as pattern" <|
        \() ->
            """
module A exposing (..)
foo =
    \\({ bish, bash } as bosh) ->
        ( bish, bosh )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\({bish} as bosh) ->
        ( bish, bosh )
"""
                    ]
    , test "should report unused patterns and unused aliases" <|
        \() ->
            """
module A exposing (..)
foo =
    \\({ bish, bash } as bosh) ->
        bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\({bish} as bosh) ->
        bish
"""
                    , Review.Test.error
                        { message = "Pattern alias `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\({bish, bash}) ->
        bish
"""
                    ]
    , test "should report unused patterns that are aliased" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(_ as bar) -> bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern `_` is not needed"
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "_"
                        }
                    ]
    , test "should report nested unused pattern aliases" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(Named ( _, ( Named bash ) as bish )) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\(Named ( _, ( Named bash ) )) ->
        bash
"""
                    ]
    ]


lambdaNamedPatternTests : List Test
lambdaNamedPatternTests =
    [ test "should report unused variables extracted out of named patterns" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(Named bish) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\(Named _) ->
        bash
"""
                    ]
    , test "should report unused nested named patterns" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(Named (Bish bish)) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\(Named (Bish _)) ->
        bash
"""
                    ]
    , test "should not report named patterns" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(Pair _ _) -> bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused named patterns in tuples" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(Singular _, Pair _ _) -> bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


lambdaRecordPatternTests : List Test
lambdaRecordPatternTests =
    [ test "should replace unused record with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    \\{ bish, bash, bosh } ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameters `bish`, `bash` and `bosh` are not used."
                        , details = [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]
                        , under = "bish, bash, bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\_ ->
        bar
"""
                    ]
    , test "should replace empty record with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    \\{} ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Record pattern is not needed"
                        , details = [ "This pattern is redundant and should be replaced with '_'." ]
                        , under = "{}"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\_ ->
        bar
"""
                    ]
    , test "should report unused record values" <|
        \() ->
            """
module A exposing (..)
foo =
    \\{ bish, bash, bosh } ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameters `bish` and `bosh` are not used."
                        , details = [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]
                        , under = "bish, bash, bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\{bash} ->
        bash
"""
                    ]
    , test "should report highlight the least amount of values possible" <|
        \() ->
            """
module A exposing (..)
foo =
    \\{ bish, bash, bosh } ->
        bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameters `bash` and `bosh` are not used."
                        , details = [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]
                        , under = "bash, bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\{bish} ->
        bish
"""
                    ]
    ]


lambdaTuplePatternTests : List Test
lambdaTuplePatternTests =
    [ test "should report unused tuple values" <|
        \() ->
            """
module A exposing (..)
foo =
    \\( bish, bash, bosh ) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\( _, bash, bosh ) ->
        bash
"""
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\( bish, bash, _ ) ->
        bash
"""
                    ]
    , test "should replace unused tuple with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    \\( _, _ ) ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "( _, _ )"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\_ ->
        bar
"""
                    ]
    , test "should replace unused threeple with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    \\( _, _, _ ) ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "( _, _, _ )"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\_ ->
        bar
"""
                    ]
    ]



--- FUNCTION PATTERN TESTS


functionAsPatternTests : List Test
functionAsPatternTests =
    [ test "should report unused pattern aliases" <|
        \() ->
            """
module A exposing (..)
foo ({ bish, bash } as bosh) =
    ( bish, bash )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report unused patterns in an as pattern" <|
        \() ->
            """
module A exposing (..)
foo ({ bish, bash } as bosh) =
    ( bish, bosh )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                    ]
    , test "should report unused patterns and unused aliases" <|
        \() ->
            """
module A exposing (..)
foo ({ bish, bash } as bosh) =
    bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                    , Review.Test.error
                        { message = "Pattern alias `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report unused patterns that are aliased" <|
        \() ->
            """
module A exposing (..)
foo (_ as bar) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern does not introduce any variable"
                        , details = [ "You should remove this pattern." ]
                        , under = "_"
                        }
                    ]
    , test "should report nested unused pattern aliases" <|
        \() ->
            """
module A exposing (..)
foo (Named ( _, ( Just bash ) as bish )) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    ]

    --test "should report unused tuple values" <|
    --        \() ->
    --            """
    --module A exposing (..)
    --foo ( bish, bash, bosh ) =
    --    bash
    --"""
    --                |> Review.Test.run rule
    --                |> Review.Test.expectErrors
    --                    [ Review.Test.error
    --                        { message = "Parameter `bish` is not used."
    --                        , details = details
    --                        , under = "bish"
    --                        }
    --                    , Review.Test.error
    --                        { message = "Parameter `bosh` is not used."
    --                        , details = details
    --                        , under = "bosh"
    --                        }
    --                    ]
    --    , test "should report unused tuple" <|
    --        \() ->
    --            """
    --module A exposing (..)
    --foo ( _, _ ) =
    --    bar
    --"""
    --                |> Review.Test.run rule
    --                |> Review.Test.expectErrors
    --                    [ Review.Test.error
    --                        { message = "Tuple pattern is not needed"
    --                        , details = [ "You should remove this pattern." ]
    --                        , under = "( _, _ )"
    --                        }
    --                        |> Review.Test.whenFixed """
    --module A exposing (..)
    --foo _ =
    --    bar
    --"""
    --                    ]
    --    , test "should report unused when it contains ()" <|
    --        \() ->
    --            """
    --module A exposing (..)
    --foo ( _, () ) =
    --    bar
    --"""
    --                |> Review.Test.run rule
    --                |> Review.Test.expectErrors
    --                    [ Review.Test.error
    --                        { message = "Tuple pattern is not needed"
    --                        , details = [ "You should remove this pattern." ]
    --                        , under = "( _, () )"
    --                        }
    --                        |> Review.Test.whenFixed """
    --module A exposing (..)
    --foo _ =
    --    bar
    --"""
    --                    ]
    --    , test "should report unused when it contains empty tuples" <|
    --        \() ->
    --            """
    --module A exposing (..)
    --foo ( _, ( _, _ ) ) =
    --    bar
    --"""
    --                |> Review.Test.run rule
    --                |> Review.Test.expectErrors
    --                    [ Review.Test.error
    --                        { message = "Tuple pattern is not needed"
    --                        , details = [ "You should remove this pattern." ]
    --                        , under = "( _, _ )"
    --                        }
    --                        |> Review.Test.whenFixed """
    --module A exposing (..)
    --foo ( _, _ ) =
    --    bar
    --"""
    --                    ]
    --    , test "should report unused threeple" <|
    --        \() ->
    --            """
    --module A exposing (..)
    --foo ( _, _, _ ) =
    --    bar
    --"""
    --                |> Review.Test.run rule
    --                |> Review.Test.expectErrors
    --                    [ Review.Test.error
    --                        { message = "Tuple pattern is not needed"
    --                        , details = [ "You should remove this pattern." ]
    --                        , under = "( _, _, _ )"
    --                        }
    --                        |> Review.Test.whenFixed """
    --module A exposing (..)
    --foo _ =
    --    bar
    --"""
    --                    ]
    ]


functionNamedPatternTests : List Test
functionNamedPatternTests =
    [ test "should report unused named patterns" <|
        \() ->
            """
module A exposing (..)
foo (Named bish) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    ]
    , test "should report unused nested named patterns" <|
        \() ->
            """
module A exposing (..)
foo (Named (Bish bish)) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    ]
    , test "should report unused named patterns with multiple segments" <|
        \() ->
            """
module A exposing (..)
foo (Pair _ _) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused named patterns in tuples" <|
        \() ->
            """
module A exposing (..)
foo (Singular _, Pair _ _) =
    bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


functionRecordPatternTests : List Test
functionRecordPatternTests =
    [ test "should report unused fields" <|
        \() ->
            """
module A exposing (..)
foo { bish, bash, bosh } =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    , Review.Test.error
                        { message = "Parameter `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report only the unused record values" <|
        \() ->
            """
module A exposing (..)
foo { bish, bash, bosh } =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                    ]
    ]


functionTuplePatternTests : List Test
functionTuplePatternTests =
    [ test "should report unused tuple values" <|
        \() ->
            """
module A exposing (..)
foo ( bish, bash, bosh ) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report unused tuple" <|
        \() ->
            """
module A exposing (..)
foo ( _, _ ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, _ )"
                        }
                    ]
    , test "should report unused when it contains ()" <|
        \() ->
            """
module A exposing (..)
foo ( _, () ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, () )"
                        }
                    ]
    , test "should report unused when it contains empty tuples" <|
        \() ->
            """
module A exposing (..)
foo ( _, ( _, _ ) ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, _ )"
                        }
                    ]
    , test "should report unused threeple" <|
        \() ->
            """
module A exposing (..)
foo ( _, _, _ ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, _, _ )"
                        }
                    ]
    ]
