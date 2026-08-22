module Core.App.Km exposing
    ( Km
    , start, fromFloat, toFloat
    , advance, difference, isBefore, isAtOrBefore, clampTo, compare, min, max
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


{-| The gap between two milestones, without direction. -}
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


compare : Km -> Km -> Order
compare (Km a) (Km b) =
    Basics.compare a b


min : Km -> Km -> Km
min (Km a) (Km b) =
    Km (Basics.min a b)


max : Km -> Km -> Km
max (Km a) (Km b) =
    Km (Basics.max a b)


{-| User facing, one decimal place. -}
toString : Km -> String
toString (Km value) =
    let
        tenths =
            round (value * 10)
    in
    String.fromInt (tenths // 10) ++ "." ++ String.fromInt (modBy 10 tenths)
