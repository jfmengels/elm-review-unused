module NoUnused.RecordFields.RangeSet exposing
    ( RangeSet
    , empty
    , fromList
    , insert
    , member
    , union
    )

import Elm.Syntax.Range exposing (Range)
import Set exposing (Set)


type RangeSet
    = RangeSet (Set String)


empty : RangeSet
empty =
    RangeSet Set.empty


fromList : List Range -> RangeSet
fromList ranges =
    ranges
        |> List.map stringifyRange
        |> Set.fromList
        |> RangeSet


insert : Range -> RangeSet -> RangeSet
insert range (RangeSet rangeSet) =
    RangeSet (Set.insert (stringifyRange range) rangeSet)


union : RangeSet -> RangeSet -> RangeSet
union (RangeSet a) (RangeSet b) =
    RangeSet (Set.union a b)


member : Range -> RangeSet -> Bool
member range (RangeSet rangeSet) =
    Set.member (stringifyRange range) rangeSet


stringifyRange : Range -> String
stringifyRange range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
        |> List.map String.fromInt
        |> String.join "-"
