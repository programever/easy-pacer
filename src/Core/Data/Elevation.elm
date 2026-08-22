module Core.Data.Elevation exposing
    ( Elevation
    , add
    , difference
    , fromMeters
    , gain
    , inMeters
    , inWholeMeters
    , loss
    , zero
    )

{-| A height above sea level, or an accumulated amount of climbing. Kept
separate from `Distance` because adding a horizontal length to a vertical one
is always a mistake.
-}


type Elevation
    = Elevation Float


zero : Elevation
zero =
    Elevation 0


fromMeters : Float -> Elevation
fromMeters m =
    Elevation m


inMeters : Elevation -> Float
inMeters (Elevation m) =
    m


inWholeMeters : Elevation -> Int
inWholeMeters e =
    round (inMeters e)


add : Elevation -> Elevation -> Elevation
add (Elevation a) (Elevation b) =
    Elevation (a + b)


{-| Signed: positive when `to` is higher than `from`.
-}
difference : Elevation -> Elevation -> Float
difference (Elevation from) (Elevation to) =
    to - from


{-| The climbing part of a signed height change, ignoring noise below the
threshold. GPS altitude jitters by a couple of meters per sample; summing raw
differences inflates total ascent well beyond the organiser's published figure.
-}
gain : Float -> Float -> Elevation
gain threshold delta =
    if delta > threshold then
        Elevation delta

    else
        Elevation 0


loss : Float -> Float -> Elevation
loss threshold delta =
    if delta < negate threshold then
        Elevation (negate delta)

    else
        Elevation 0
