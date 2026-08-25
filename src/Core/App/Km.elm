module Core.App.Km exposing
    ( Km
    , advance
    , clampTo
    , difference
    , fromFloat
    , isAtOrBefore
    , isBefore
    , start
    , toEditString
    , toFloat
    , toString
    )

{-| A position measured along the route from the start line.

This is deliberately NOT `Distance`. A `Km` is a milestone, a `Distance` is a
gap between two milestones. Mixing them produced a real bug once: the point used
to guide a lost runner back was taken from a list that had already been filtered
to "ahead of the runner", so the shortest way back was never considered.
`difference` returning `Distance` makes that class of mistake a type error.

-}

import Core.Data.Distance as Distance exposing (Distance)


type Km
    = Km Float


start : Km
start =
    Km 0


fromFloat : Float -> Km
fromFloat value =
    Km (Basics.max 0 value)


toFloat : Km -> Float
toFloat (Km value) =
    value


advance : Distance -> Km -> Km
advance gap (Km value) =
    Km (value + Distance.inKilometers gap)


{-| The gap between two milestones, without direction.
-}
difference : Km -> Km -> Distance
difference (Km a) (Km b) =
    Distance.fromKilometers (abs (b - a))


isBefore : Km -> Km -> Bool
isBefore (Km limit) (Km value) =
    value < limit


isAtOrBefore : Km -> Km -> Bool
isAtOrBefore (Km limit) (Km value) =
    value <= limit


clampTo : Km -> Km -> Km
clampTo (Km limit) (Km value) =
    Km (clamp 0 limit value)


{-| User facing, one decimal place. For reading: a runner glancing at a station
does not want three decimals of a number they cannot run to.
-}
toString : Km -> String
toString (Km value) =
    let
        tenths =
            round (value * 10)
    in
    String.fromInt (tenths // 10) ++ "." ++ String.fromInt (modBy 10 tenths)


{-| What an edit box starts from, to the metre and with no trailing zeros.

Separate from `toString` because an edit box has a duty a label does not: what
it shows must be what it would save. Filling it from `toString` silently turned
a station typed as 8.555 into 8.5 the moment the box lost focus.

-}
toEditString : Km -> String
toEditString (Km value) =
    let
        thousandths =
            round (value * 1000)

        fraction =
            modBy 1000 thousandths
    in
    if fraction == 0 then
        String.fromInt (thousandths // 1000)

    else
        String.fromInt (thousandths // 1000)
            ++ "."
            ++ withoutTrailingZeros (String.padLeft 3 '0' (String.fromInt fraction))


{-| Never called with all zeros: the caller has already ruled that out, so this
cannot eat the whole string.
-}
withoutTrailingZeros : String -> String
withoutTrailingZeros text =
    if String.endsWith "0" text then
        withoutTrailingZeros (String.dropRight 1 text)

    else
        text
