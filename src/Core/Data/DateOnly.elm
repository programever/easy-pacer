module Core.Data.DateOnly exposing
    ( DateOnly
    , fromString, fromParts, fromPosix, addDays
    , at, toString, toIsoString
    )

{-| A calendar day with no time attached. Exists so a race can be set up days
in advance, and so a time of day can be anchored to a real moment.
-}

import Core.Data.Clock as Clock exposing (Clock)
import Time


type DateOnly
    = DateOnly { year : Int, month : Int, day : Int }


fromParts : Int -> Int -> Int -> Maybe DateOnly
fromParts year month day =
    if month >= 1 && month <= 12 && day >= 1 && day <= daysInMonth year month then
        Just (DateOnly { year = year, month = month, day = day })

    else
        Nothing


{-| Accepts "18/8", "18-08", "18.08.2026", "18/08/26". A missing year means the
current year, which the caller supplies.
-}
fromString : Int -> String -> Maybe DateOnly
fromString currentYear raw =
    let
        parts =
            String.trim raw
                |> String.map
                    (\c ->
                        if c == '/' || c == '-' || c == '.' then
                            '|'

                        else
                            c
                    )
                |> String.split "|"
                |> List.filter (\p -> p /= "")
                |> List.map String.toInt
    in
    case parts of
        [ Just day, Just month ] ->
            fromParts currentYear month day

        [ Just day, Just month, Just year ] ->
            fromParts (expandYear year) month day

        _ ->
            Nothing


expandYear : Int -> Int
expandYear year =
    if year < 100 then
        2000 + year

    else
        year


{-| Anchor a time of day to this date, in the runner's own zone. -}
at : Time.Zone -> DateOnly -> Clock -> Time.Posix
at zone (DateOnly parts) clock =
    let
        days =
            daysFromCivil parts.year parts.month parts.day

        millis =
            (days * 86400000)
                + (Clock.minutesFromMidnight clock * 60000)
                - offsetMillis zone days
    in
    Time.millisToPosix millis


{-| Elm's `Time.Zone` cannot be inverted directly, so the offset is recovered by
asking the zone what local hour a known UTC instant maps to. Vietnam has no
daylight saving, so a single probe is exact here; elsewhere it is correct except
within one hour of a transition.
-}
offsetMillis : Time.Zone -> Int -> Int
offsetMillis zone days =
    let
        probe =
            Time.millisToPosix (days * 86400000)

        localMinutes =
            Time.toHour zone probe * 60 + Time.toMinute zone probe
    in
    if localMinutes > 720 then
        (localMinutes - 1440) * 60000

    else
        localMinutes * 60000


addDays : Int -> DateOnly -> DateOnly
addDays n ((DateOnly parts) as date) =
    if n <= 0 then
        date

    else
        let
            next =
                if parts.day < daysInMonth parts.year parts.month then
                    { parts | day = parts.day + 1 }

                else if parts.month < 12 then
                    { parts | month = parts.month + 1, day = 1 }

                else
                    { year = parts.year + 1, month = 1, day = 1 }
        in
        addDays (n - 1) (DateOnly next)


fromPosix : Time.Zone -> Time.Posix -> DateOnly
fromPosix zone posix =
    DateOnly
        { year = Time.toYear zone posix
        , month = monthNumber (Time.toMonth zone posix)
        , day = Time.toDay zone posix
        }


{-| User facing, Vietnamese day-first order. -}
toString : DateOnly -> String
toString (DateOnly parts) =
    pad parts.day ++ "/" ++ pad parts.month ++ "/" ++ String.fromInt parts.year


{-| Machine facing, for storage. -}
toIsoString : DateOnly -> String
toIsoString (DateOnly parts) =
    String.fromInt parts.year ++ "-" ++ pad parts.month ++ "-" ++ pad parts.day


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        2 ->
            if isLeapYear year then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 4 year == 0 && modBy 100 year /= 0) || modBy 400 year == 0


{-| Days since the Unix epoch, by Howard Hinnant's civil calendar algorithm. -}
daysFromCivil : Int -> Int -> Int -> Int
daysFromCivil year month day =
    let
        y =
            if month <= 2 then
                year - 1

            else
                year

        era =
            (if y >= 0 then
                y

             else
                y - 399
            )
                // 400

        yoe =
            y - era * 400

        mp =
            modBy 12 (month + 9)

        doy =
            (153 * mp + 2) // 5 + day - 1

        doe =
            yoe * 365 + yoe // 4 - yoe // 100 + doy
    in
    era * 146097 + doe - 719468


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


pad : Int -> String
pad n =
    String.padLeft 2 '0' (String.fromInt n)
