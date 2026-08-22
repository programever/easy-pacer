module Core.Data.Distance exposing
    ( Distance
    , add
    , compare
    , fromKilometers
    , fromMeters
    , inKilometers
    , inMeters
    , inWholeMeters
    , isGreaterThan
    , largest
    , scale
    , subtract
    , zero
    )

{-| A length. Always stored in kilometers internally, but the constructors and
accessors force the caller to name the unit, so a kilometer value can never be
silently used where meters were meant.

A `Distance` is a length BETWEEN two things. A position along a route is a
different concept with a different type: see `Core.App.Km`.

-}


type Distance
    = Distance Float


zero : Distance
zero =
    Distance 0


fromKilometers : Float -> Distance
fromKilometers km =
    Distance (abs km)


fromMeters : Float -> Distance
fromMeters m =
    Distance (abs m / 1000)


inKilometers : Distance -> Float
inKilometers (Distance km) =
    km


inMeters : Distance -> Float
inMeters (Distance km) =
    km * 1000


inWholeMeters : Distance -> Int
inWholeMeters d =
    round (inMeters d)


add : Distance -> Distance -> Distance
add (Distance a) (Distance b) =
    Distance (a + b)


{-| Never negative: distances have no direction.
-}
subtract : Distance -> Distance -> Distance
subtract (Distance a) (Distance b) =
    Distance (max 0 (a - b))


scale : Float -> Distance -> Distance
scale factor (Distance km) =
    Distance (abs (factor * km))


compare : Distance -> Distance -> Order
compare (Distance a) (Distance b) =
    Basics.compare a b


isGreaterThan : Distance -> Distance -> Bool
isGreaterThan (Distance limit) (Distance value) =
    value > limit


largest : Distance -> Distance -> Distance
largest (Distance a) (Distance b) =
    Distance (max a b)
