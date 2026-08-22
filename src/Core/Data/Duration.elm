module Core.Data.Duration exposing
    ( Duration
    , fromMinutes, fromMillis, between
    , inMinutes, inHours, isNegative
    , toCompactString, toSignedString
    )

{-| A signed span of time. Negative is meaningful here: a cutoff can be in the
past, and the display must be able to say so.
-}

import Time


type Duration
    = Duration Float


fromMinutes : Float -> Duration
fromMinutes m =
    Duration m


fromMillis : Float -> Duration
fromMillis ms =
    Duration (ms / 60000)


between : Time.Posix -> Time.Posix -> Duration
between from to =
    Duration (toFloat (Time.posixToMillis to - Time.posixToMillis from) / 60000)


inMinutes : Duration -> Float
inMinutes (Duration m) =
    m


inHours : Duration -> Float
inHours (Duration m) =
    m / 60


isNegative : Duration -> Bool
isNegative (Duration m) =
    m < 0


{-| User facing. Vietnamese, because this string is rendered as-is. -}
toCompactString : Duration -> String
toCompactString (Duration raw) =
    let
        total =
            abs raw

        hours =
            floor (total / 60)

        minutes =
            round total - (hours * 60)

        sign =
            if raw < 0 then
                "-"

            else
                ""
    in
    if hours > 0 then
        sign ++ String.fromInt hours ++ "h" ++ pad minutes

    else
        sign ++ String.fromInt minutes ++ "ph"


{-| Always carries an explicit sign, for "ahead of / behind the cutoff". -}
toSignedString : Duration -> String
toSignedString ((Duration raw) as duration) =
    if raw >= 0 then
        "+" ++ String.dropLeft 0 (toCompactString duration)

    else
        toCompactString duration


pad : Int -> String
pad n =
    String.padLeft 2 '0' (String.fromInt n)
