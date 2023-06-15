module String.Extra exposing (isCapitalized)

{-| Some utilities.
-}


{-| Check if the first character of a string is upper case, unicode aware.

Note that `Char.isUpper` is ASCII only, while `Char.isUpper` works outside ASCII.

-}
isCapitalized : String -> Bool
isCapitalized string =
    case String.uncons string of
        Just ( char, _ ) ->
            -- 1. The character has a lower and upper variant.
            (Char.toUpper char /= Char.toLower char)
                -- 2. The character is its upper variant.
                && (Char.toUpper char == char)

        Nothing ->
            False
