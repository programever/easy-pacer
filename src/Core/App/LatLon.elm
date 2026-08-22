module Core.App.LatLon exposing
    ( LatLon, Bearing, Compass(..), Plane, Projection
    , haversine, bearing, compass
    , projection, project, planeDistance, projectOntoSegment
    , bearingDegrees, compassLabel, compassArrow
    , toCoordinateString
    )

{-| Geographic positions and the flat projection used for everything that has to
be fast: snapping a fix onto the route, drawing the map, hit testing.

Distances on a trail route span a few tens of kilometers at most, so an
equirectangular projection anchored at the route's own latitude is accurate to
well under a meter here, and it turns "closest point on the route" into plain
two dimensional geometry.
-}

import Core.Data.Distance as Distance exposing (Distance)


type alias LatLon =
    { lat : Float, lon : Float }


type Bearing
    = Bearing Float


type Compass
    = North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest


{-| A point in the flat projection, in kilometers. North is negative Y so that
the map draws with north up without a further flip.
-}
type alias Plane =
    { x : Float, y : Float }


{-| Carries the reference latitude the projection was built for. A `Plane` is
only comparable with another `Plane` from the same `Projection`.
-}
type Projection
    = Projection Float


earthRadiusKm : Float
earthRadiusKm =
    6371


haversine : LatLon -> LatLon -> Distance
haversine from to =
    let
        dLat =
            degrees (to.lat - from.lat)

        dLon =
            degrees (to.lon - from.lon)

        a =
            (sin (dLat / 2) ^ 2)
                + (cos (degrees from.lat) * cos (degrees to.lat) * (sin (dLon / 2) ^ 2))
    in
    Distance.fromKilometers (2 * earthRadiusKm * asin (sqrt (min 1 a)))


bearing : LatLon -> LatLon -> Bearing
bearing from to =
    let
        phiFrom =
            degrees from.lat

        phiTo =
            degrees to.lat

        dLon =
            degrees (to.lon - from.lon)

        y =
            sin dLon * cos phiTo

        x =
            (cos phiFrom * sin phiTo) - (sin phiFrom * cos phiTo * cos dLon)

        raw =
            atan2 y x * 180 / pi
    in
    Bearing (toFloat (modBy 360 (round (raw + 360))))


bearingDegrees : Bearing -> Int
bearingDegrees (Bearing degreesValue) =
    round degreesValue


compass : Bearing -> Compass
compass (Bearing degreesValue) =
    case modBy 8 (round (degreesValue / 45)) of
        0 ->
            North

        1 ->
            NorthEast

        2 ->
            East

        3 ->
            SouthEast

        4 ->
            South

        5 ->
            SouthWest

        6 ->
            West

        _ ->
            NorthWest


{-| User facing. -}
compassLabel : Compass -> String
compassLabel direction =
    case direction of
        North ->
            "Bắc"

        NorthEast ->
            "Đông Bắc"

        East ->
            "Đông"

        SouthEast ->
            "Đông Nam"

        South ->
            "Nam"

        SouthWest ->
            "Tây Nam"

        West ->
            "Tây"

        NorthWest ->
            "Tây Bắc"


compassArrow : Compass -> String
compassArrow direction =
    case direction of
        North ->
            "↑"

        NorthEast ->
            "↗"

        East ->
            "→"

        SouthEast ->
            "↘"

        South ->
            "↓"

        SouthWest ->
            "↙"

        West ->
            "←"

        NorthWest ->
            "↖"


projection : Float -> Projection
projection referenceLatitude =
    Projection referenceLatitude


project : Projection -> LatLon -> Plane
project (Projection referenceLatitude) position =
    { x = position.lon * 111.32 * cos (degrees referenceLatitude)
    , y = negate (position.lat * 110.57)
    }


planeDistance : Plane -> Plane -> Distance
planeDistance a b =
    Distance.fromKilometers (sqrt (((a.x - b.x) ^ 2) + ((a.y - b.y) ^ 2)))


{-| Closest point on the segment `a`-`b` to `point`, as a fraction along it.
Clamped to the segment, so the result is never past either end.
-}
projectOntoSegment : Plane -> Plane -> Plane -> Float
projectOntoSegment a b point =
    let
        dx =
            b.x - a.x

        dy =
            b.y - a.y

        lengthSquared =
            (dx * dx) + (dy * dy)
    in
    if lengthSquared <= 0 then
        0

    else
        clamp 0 1 ((((point.x - a.x) * dx) + ((point.y - a.y) * dy)) / lengthSquared)


{-| Six decimal places is about 10 cm, which is far finer than any phone fix and
short enough to survive being retyped into a text message.
-}
toCoordinateString : LatLon -> String
toCoordinateString position =
    round6 position.lat ++ ", " ++ round6 position.lon


round6 : Float -> String
round6 value =
    let
        scaled =
            round (value * 1000000)

        sign =
            if scaled < 0 then
                "-"

            else
                ""

        magnitude =
            abs scaled
    in
    sign
        ++ String.fromInt (magnitude // 1000000)
        ++ "."
        ++ String.padLeft 6 '0' (String.fromInt (modBy 1000000 magnitude))
