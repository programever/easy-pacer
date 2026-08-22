module Core.Data.Clock exposing
    ( Clock
    , midnight
    , fromString, fromHourMinute, fromPosix
    , hour, minute, minutesFromMidnight
    , toString
    )

{-| A time of day, 0 to 1439 minutes past midnight. It deliberately carries no
date: a cutoff written "01:00" on a race sheet is meaningless until anchored to
a start moment, and forcing that anchoring through `Core.Data.DateOnly.at`
keeps overnight races correct.
-}

import Time


type Clock
    = Clock Int


midnight : Clock
midnight =
    Clock 0


fromHourMinute : Int -> Int -> Maybe Clock
fromHourMinute h m =
    if h >= 0 && h <= 23 && m >= 0 && m <= 59 then
        Just (Clock (h * 60 + m))

    else
        Nothing


{-| Accepts the shapes runners actually type on a phone: "0530", "5:30", "530",
"5.30", "18h30". Anything else is rejected rather than guessed at.
-}
fromString : String -> Maybe Clock
fromString raw =
    let
        trimmed =
            String.trim raw

        separated =
            splitOnSeparator trimmed

        digitsOnly =
            String.filter Char.isDigit trimmed
    in
    case separated of
        Just ( h, m ) ->
            fromHourMinute h m

        Nothing ->
            case String.length digitsOnly of
                4 ->
                    pair (String.left 2 digitsOnly) (String.right 2 digitsOnly)

                3 ->
                    pair (String.left 1 digitsOnly) (String.right 2 digitsOnly)

                _ ->
                    Nothing


splitOnSeparator : String -> Maybe ( Int, Int )
splitOnSeparator text =
    let
        parts =
            String.split "" text
                |> List.map
                    (\c ->
                        if c == ":" || c == "." || c == "h" || c == " " then
                            "|"

                        else
                            c
                    )
                |> String.concat
                |> String.split "|"
                |> List.filter (\p -> p /= "")
    in
    case parts of
        [ left, right ] ->
            Maybe.map2 Tuple.pair (String.toInt left) (String.toInt right)

        _ ->
            Nothing


pair : String -> String -> Maybe Clock
pair left right =
    Maybe.map2 Tuple.pair (String.toInt left) (String.toInt right)
        |> Maybe.andThen (\( h, m ) -> fromHourMinute h m)


fromPosix : Time.Zone -> Time.Posix -> Clock
fromPosix zone posix =
    Clock (Time.toHour zone posix * 60 + Time.toMinute zone posix)


hour : Clock -> Int
hour (Clock total) =
    total // 60


minute : Clock -> Int
minute (Clock total) =
    modBy 60 total


minutesFromMidnight : Clock -> Int
minutesFromMidnight (Clock total) =
    total


toString : Clock -> String
toString clock =
    pad (hour clock) ++ ":" ++ pad (minute clock)


pad : Int -> String
pad n =
    String.padLeft 2 '0' (String.fromInt n)
