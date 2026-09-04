module Core.App.Route exposing
    ( Error
    , Point
    , Route
    , Sample
    , Waypoint
    , ascentBetween
    , atKm
    , descentBetween
    , elevationRange
    , errorMessage
    , fromSamples
    , points
    , projection
    , remainingAscent
    , remainingDescent
    , slice
    , totalAscent
    , totalDescent
    , totalKm
    , waypoints
    )

{-| The course, as read from a GPX file: a chain of points with cumulative
distance and cumulative climb already worked out, plus the flat projection every
geometric question is answered in.

Building a `Route` is the only place raw GPX numbers are trusted, and it happens
once. Everything downstream reads a `Route` and cannot get the arithmetic wrong.

-}

import Array exposing (Array)
import Core.App.Km as Km exposing (Km)
import Core.App.LatLon as LatLon exposing (LatLon, Plane, Projection)
import Core.Data.Elevation as Elevation exposing (Elevation)
import Core.Data.NonEmpty as NonEmpty exposing (NonEmpty)


type Route
    = Route
        { points : NonEmpty Point
        , indexed : Array Point
        , totalKm : Km
        , totalAscent : Elevation
        , totalDescent : Elevation
        , waypoints : List Waypoint
        , projection : Projection
        }


type alias Point =
    { position : LatLon
    , elevation : Elevation
    , km : Km
    , ascentSoFar : Elevation
    , descentSoFar : Elevation
    , plane : Plane
    }


{-| One raw trackpoint, exactly as it comes out of the GPX file.
-}
type alias Sample =
    { lat : Float, lon : Float, ele : Float }


type alias Waypoint =
    { name : String, position : LatLon }


type Error
    = TooFewPoints


{-| Number of points kept after thinning. Enough to keep every switchback on a
40 km course, few enough that snapping a fix stays instant on a phone.
-}
maxPoints : Int
maxPoints =
    3000


{-| Metres of altitude change below which a step is treated as sensor noise.
-}
climbThreshold : Float
climbThreshold =
    0.5


fromSamples : List Sample -> List Waypoint -> Result Error Route
fromSamples rawSamples rawWaypoints =
    case NonEmpty.fromList (thin (List.filter isFinite rawSamples)) of
        Nothing ->
            Err TooFewPoints

        Just thinned ->
            if NonEmpty.length thinned < 2 then
                Err TooFewPoints

            else
                Ok (build thinned rawWaypoints)


isFinite : Sample -> Bool
isFinite sample =
    not (isNaN sample.lat || isNaN sample.lon || isInfinite sample.lat || isInfinite sample.lon)


{-| Keep at most `maxPoints`, always including the last one so the total
distance is not cut short.
-}
thin : List Sample -> List Sample
thin samples =
    let
        count =
            List.length samples
    in
    if count <= maxPoints then
        samples

    else
        let
            stride =
                (count // maxPoints) + 1
        in
        (List.indexedMap Tuple.pair samples
            |> List.filter (\( index, _ ) -> modBy stride index == 0)
            |> List.map Tuple.second
        )
            ++ List.drop (count - 1) samples


build : NonEmpty Sample -> List Waypoint -> Route
build samples rawWaypoints =
    let
        asList =
            NonEmpty.toList samples

        referenceLatitude =
            List.sum (List.map .lat asList) / Basics.toFloat (List.length asList)

        proj =
            LatLon.projection referenceLatitude

        smoothed =
            smoothElevations asList

        accumulated =
            List.foldl (accumulate proj) [] smoothed
                |> List.reverse

        finalPoints =
            Maybe.withDefault
                (NonEmpty.singleton (seedPoint proj (NonEmpty.head samples)))
                (NonEmpty.fromList accumulated)

        last =
            List.foldl (\point _ -> point) (NonEmpty.head finalPoints) (NonEmpty.toList finalPoints)
    in
    Route
        { points = finalPoints
        , indexed = Array.fromList (NonEmpty.toList finalPoints)
        , totalKm = last.km
        , totalAscent = last.ascentSoFar
        , totalDescent = last.descentSoFar
        , waypoints = rawWaypoints
        , projection = proj
        }


{-| A five point moving average over altitude. Raw barometric samples jitter by
a couple of metres each, and summing them unsmoothed inflates total ascent to
roughly half again the organiser's published figure, which makes the whole
elevation column look wrong to a runner checking it against the race sheet.
-}
smoothElevations : List Sample -> List Sample
smoothElevations samples =
    let
        elevations =
            Array.fromList (List.map .ele samples)

        count =
            Array.length elevations

        average index =
            let
                low =
                    Basics.max 0 (index - 2)

                high =
                    Basics.min (count - 1) (index + 2)

                total =
                    List.range low high
                        |> List.filterMap (\i -> Array.get i elevations)
            in
            case total of
                [] ->
                    0

                _ ->
                    List.sum total / Basics.toFloat (List.length total)
    in
    List.indexedMap (\index sample -> { sample | ele = average index }) samples


seedPoint : Projection -> Sample -> Point
seedPoint proj sample =
    let
        position =
            { lat = sample.lat, lon = sample.lon }
    in
    { position = position
    , elevation = Elevation.fromMeters sample.ele
    , km = Km.start
    , ascentSoFar = Elevation.zero
    , descentSoFar = Elevation.zero
    , plane = LatLon.project proj position
    }


{-| Folds newest-first; `build` reverses at the end.
-}
accumulate : Projection -> Sample -> List Point -> List Point
accumulate proj sample acc =
    case acc of
        [] ->
            [ seedPoint proj sample ]

        previous :: _ ->
            let
                position =
                    { lat = sample.lat, lon = sample.lon }

                gap =
                    LatLon.haversine previous.position position

                delta =
                    Elevation.difference previous.elevation (Elevation.fromMeters sample.ele)
            in
            { position = position
            , elevation = Elevation.fromMeters sample.ele
            , km = Km.advance gap previous.km
            , ascentSoFar = Elevation.add previous.ascentSoFar (Elevation.gain climbThreshold delta)
            , descentSoFar = Elevation.add previous.descentSoFar (Elevation.loss climbThreshold delta)
            , plane = LatLon.project proj position
            }
                :: acc


points : Route -> NonEmpty Point
points (Route route) =
    route.points


totalKm : Route -> Km
totalKm (Route route) =
    route.totalKm


totalAscent : Route -> Elevation
totalAscent (Route route) =
    route.totalAscent


totalDescent : Route -> Elevation
totalDescent (Route route) =
    route.totalDescent


waypoints : Route -> List Waypoint
waypoints (Route route) =
    route.waypoints


projection : Route -> Projection
projection (Route route) =
    route.projection


{-| Total: a milestone outside the course is clamped to its ends, so callers
never handle a `Maybe` for a question that always has a sensible answer.
-}
atKm : Route -> Km -> Point
atKm ((Route route) as wrapped) wanted =
    let
        target =
            Km.clampTo route.totalKm wanted

        first =
            NonEmpty.head (points wrapped)

        index =
            search route.indexed target 0 (Array.length route.indexed - 1)
    in
    case ( Array.get (index - 1) route.indexed, Array.get index route.indexed ) of
        ( Just before, Just after ) ->
            interpolate before after target

        ( Nothing, Just after ) ->
            after

        _ ->
            first


{-| Index of the first point at or past `target`. The points are sorted by km by
construction, which is what makes the halving valid.
-}
search : Array Point -> Km -> Int -> Int -> Int
search indexed target low high =
    if low >= high then
        low

    else
        let
            middle =
                low + ((high - low) // 2)
        in
        case Array.get middle indexed of
            Nothing ->
                low

            Just point ->
                if Km.isBefore target point.km then
                    search indexed target (middle + 1) high

                else
                    search indexed target low middle


interpolate : Point -> Point -> Km -> Point
interpolate before after target =
    let
        span =
            Km.toFloat after.km - Km.toFloat before.km

        fraction =
            if span <= 0 then
                0

            else
                (Km.toFloat target - Km.toFloat before.km) / span

        blend from to =
            from + ((to - from) * fraction)
    in
    { position =
        { lat = blend before.position.lat after.position.lat
        , lon = blend before.position.lon after.position.lon
        }
    , elevation =
        Elevation.fromMeters
            (blend (Elevation.inMeters before.elevation) (Elevation.inMeters after.elevation))
    , km = target
    , ascentSoFar =
        Elevation.fromMeters
            (blend (Elevation.inMeters before.ascentSoFar) (Elevation.inMeters after.ascentSoFar))
    , descentSoFar =
        Elevation.fromMeters
            (blend (Elevation.inMeters before.descentSoFar) (Elevation.inMeters after.descentSoFar))
    , plane =
        { x = blend before.plane.x after.plane.x
        , y = blend before.plane.y after.plane.y
        }
    }


{-| The stretch of course between two milestones: the exact point at `from`,
every recorded point strictly between, and the exact point at `to`. Both ends
are clamped to the course. A `to` at or before `from` yields just the point at
`from`, so a caller drawing the stretch draws nothing there rather than a line
running backwards.
-}
slice : Route -> Km -> Km -> List Point
slice route from to =
    let
        start =
            atKm route from

        end =
            atKm route to
    in
    if Km.isAtOrBefore start.km end.km then
        [ start ]

    else
        start
            :: List.filter
                (\point -> Km.isBefore point.km start.km && Km.isBefore end.km point.km)
                (NonEmpty.toList (points route))
            ++ [ end ]


ascentBetween : Route -> Km -> Km -> Elevation
ascentBetween route from to =
    if Km.isAtOrBefore from to then
        Elevation.zero

    else
        Elevation.fromMeters
            (Elevation.inMeters (atKm route to).ascentSoFar
                - Elevation.inMeters (atKm route from).ascentSoFar
            )


descentBetween : Route -> Km -> Km -> Elevation
descentBetween route from to =
    if Km.isAtOrBefore from to then
        Elevation.zero

    else
        Elevation.fromMeters
            (Elevation.inMeters (atKm route to).descentSoFar
                - Elevation.inMeters (atKm route from).descentSoFar
            )


remainingAscent : Route -> Km -> Elevation
remainingAscent route from =
    ascentBetween route from (totalKm route)


remainingDescent : Route -> Km -> Elevation
remainingDescent route from =
    descentBetween route from (totalKm route)


{-| Lowest and highest altitude on the course, for scaling the profile chart.
-}
elevationRange : Route -> ( Float, Float )
elevationRange route =
    let
        metres =
            List.map (\point -> Elevation.inMeters point.elevation) (NonEmpty.toList (points route))

        low =
            List.minimum metres |> Maybe.withDefault 0

        high =
            List.maximum metres |> Maybe.withDefault (low + 1)
    in
    ( low, Basics.max high (low + 1) )


{-| User facing.
-}
errorMessage : Error -> String
errorMessage error =
    case error of
        TooFewPoints ->
            "File này không có dữ liệu đường chạy. Kiểm tra lại xem có đúng file GPX không."
