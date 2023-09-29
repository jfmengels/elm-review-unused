module List.Extra exposing (dictToListMap, find, indexedFilterMap, insertAllJusts)

{-| Some utilities.
-}

import Dict exposing (Dict)
import Set exposing (Set)


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ] == Just 6

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


indexedFilterMap : (Int -> a -> Maybe b) -> Int -> List a -> List b -> List b
indexedFilterMap predicate index list acc =
    case list of
        [] ->
            acc

        x :: xs ->
            indexedFilterMap predicate
                (index + 1)
                xs
                (case predicate index x of
                    Just b ->
                        b :: acc

                    Nothing ->
                        acc
                )


dictToListMap : (k -> v -> a) -> Dict k v -> List a -> List a
dictToListMap fn dict baseAcc =
    Dict.foldr (\k v acc -> fn k v :: acc) baseAcc dict


insertAllJusts : List ( a, Maybe comparable ) -> Set comparable -> Set comparable
insertAllJusts list set =
    Set.union
        (Set.fromList (List.filterMap Tuple.second list))
        set
