module Core.Data.Distance exposing
    ( Distance
    , fromKilometers
    , fromMeters
    , inKilometers
    , inMeters
    , inWholeMeters
    , isGreaterThan
    )

{-| A length. Always stored in kilometers internally, but the constructors and
accessors force the caller to name the unit, so a kilometer value can never be
silently used where meters were meant.

A `Distance` is a length BETWEEN two things. A position along a route is a
different concept with a different type: see `Core.App.Km`.

-}


type Distance
    = Distance Float


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


isGreaterThan : Distance -> Distance -> Bool
isGreaterThan (Distance limit) (Distance value) =
    value > limit
