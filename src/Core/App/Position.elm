module Core.App.Position exposing
    ( Candidate
    , Resolution(..)
    , RouteState(..)
    , acceptableAccuracy
    , acceptableDeviation
    , candidateAt
    , candidateDeviation
    , candidateKm
    , candidateSnap
    , candidates
    , isTrustworthy
    , offRouteThreshold
    , resolve
    , routeState
    )

{-| Turning one GPS fix into two separate answers.

A trail course crosses itself, doubles back and loops, so a single coordinate
matches several milestones. The two questions asked of a fix need different
answers and must not share one:

  - how far have I run: the milestone ahead of where I was, filtered by what a
    human can physically have covered since the last fix;
  - where is the way back: the nearest point on the WHOLE course, including
    sections already run, because when you are lost the shortest way back is
    often behind you.

Returning both in `Resolved` forces every caller to pick the right one.

-}

import Array
import Core.App.Km as Km exposing (Km)
import Core.App.LatLon as LatLon exposing (LatLon, Plane)
import Core.App.Progress as Progress exposing (Fix, Progress)
import Core.App.Route as Route exposing (Point, Route)
import Core.Data.Distance as Distance exposing (Distance)
import Core.Data.Duration as Duration
import Core.Data.NonEmpty as NonEmpty exposing (NonEmpty)
import Time


type Candidate
    = Candidate { km : Km, deviation : Distance, snap : LatLon }


type Resolution
    = Resolved { progress : Candidate, nearest : Candidate }
    | Ambiguous { options : NonEmpty Candidate, nearest : Candidate }


type RouteState
    = OnRoute
    | Uncertain
    | OffRoute


{-| Beyond this a runner is treated as having left the course, provided the
measurement is precise enough to say so.
-}
offRouteThreshold : Distance
offRouteThreshold =
    Distance.fromMeters 60


{-| A fix must be at least this precise before it is allowed to move the
runner's milestone. Under tree cover a phone commonly reports 20 to 40 m;
past this the snapped milestone can be a tenth of a kilometre out, and a
number that can be wrong by that much is worse than the old one. The runner
is told, and can stand somewhere clearer and try again, or type the km.
-}
acceptableAccuracy : Distance
acceptableAccuracy =
    Distance.fromMeters 50


{-| Further than this from the course, a snapped milestone means nothing: the
runner is lost, and the job is guidance back, not a km update.
-}
acceptableDeviation : Distance
acceptableDeviation =
    Distance.fromMeters 250


{-| Whether a fix is good enough to move the milestone. Guidance back to the
course is given regardless; only the km is protected.
-}
isTrustworthy : Fix -> Candidate -> Bool
isTrustworthy fix (Candidate nearest) =
    not (Distance.isGreaterThan acceptableAccuracy fix.accuracy)
        && not (Distance.isGreaterThan acceptableDeviation nearest.deviation)


{-| No runner covers ground faster than this, so any milestone implying it can
be ruled out. Generous on purpose: it is a sanity bound, not a pace model.
-}
maxSpeedKmPerHour : Float
maxSpeedKmPerHour =
    18


{-| GPS noise allowance for going backwards along the course.
-}
backwardTolerance : Distance
backwardTolerance =
    Distance.fromMeters 300


{-| Two candidates closer than this in deviation are treated as equally good,
and something other than distance has to choose between them.
-}
tieWindow : Distance
tieWindow =
    Distance.fromMeters 50


{-| Candidates whose milestones sit closer together than this are the same place
seen twice by the scan, not two different passes.
-}
mergeWindow : Float
mergeWindow =
    0.4


{-| Builds a candidate directly. Needed because `Candidate` is opaque: tests
have to be able to state "a fix eighty metres off course" without inventing a
whole route to put it on.
-}
candidateAt : Km -> Distance -> LatLon -> Candidate
candidateAt km deviation snap =
    Candidate { km = km, deviation = deviation, snap = snap }


candidateKm : Candidate -> Km
candidateKm (Candidate c) =
    c.km


candidateDeviation : Candidate -> Distance
candidateDeviation (Candidate c) =
    c.deviation


candidateSnap : Candidate -> LatLon
candidateSnap (Candidate c) =
    c.snap


{-| Every local minimum of "distance from the fix to the course", sorted nearest
first. Each time the course passes the runner it contributes exactly one
candidate, instead of dozens of nearly identical ones.
-}
candidates : Route -> LatLon -> List Candidate
candidates route position =
    let
        here =
            LatLon.project (Route.projection route) position

        pointList =
            NonEmpty.toList (Route.points route)

        segments =
            List.map2 (\a b -> nearestOnSegment here a b)
                pointList
                (List.drop 1 pointList)
    in
    localMinima segments
        |> List.sortBy (\(Candidate c) -> Distance.inKilometers c.deviation)


nearestOnSegment : Plane -> Point -> Point -> Candidate
nearestOnSegment here before after =
    let
        fraction =
            LatLon.projectOntoSegment before.plane after.plane here

        blend from to =
            from + ((to - from) * fraction)

        closest =
            { x = blend before.plane.x after.plane.x
            , y = blend before.plane.y after.plane.y
            }
    in
    Candidate
        { km = Km.fromFloat (blend (Km.toFloat before.km) (Km.toFloat after.km))
        , deviation = LatLon.planeDistance here closest
        , snap =
            { lat = blend before.position.lat after.position.lat
            , lon = blend before.position.lon after.position.lon
            }
        }


localMinima : List Candidate -> List Candidate
localMinima list =
    let
        indexed =
            Array.fromList list

        deviationAt index =
            Array.get index indexed
                |> Maybe.map (\(Candidate c) -> Distance.inKilometers c.deviation)
                |> Maybe.withDefault (1 / 0)

        keep index ((Candidate c) as candidate) =
            if
                Distance.inKilometers c.deviation
                    <= deviationAt (index - 1)
                    && Distance.inKilometers c.deviation
                    <= deviationAt (index + 1)
            then
                Just candidate

            else
                Nothing
    in
    List.indexedMap keep list
        |> List.filterMap identity
        |> List.foldl mergeNeighbours []
        |> List.reverse


mergeNeighbours : Candidate -> List Candidate -> List Candidate
mergeNeighbours ((Candidate current) as candidate) acc =
    case acc of
        ((Candidate previous) as head) :: rest ->
            if abs (Km.toFloat previous.km - Km.toFloat current.km) < mergeWindow then
                if Distance.inKilometers current.deviation < Distance.inKilometers previous.deviation then
                    candidate :: rest

                else
                    head :: rest

            else
                candidate :: acc

        [] ->
            [ candidate ]


{-| The whole disambiguation, in one place.

Order of elimination:

1.  Only if a previous position is known: no going backwards beyond GPS noise,
    nothing faster than a human, and the milestone must advance by at least the
    straight line distance covered since the last fix — a course is never
    shorter than the crow flies, which is what rules out the wrong lap.
2.  If several survivors are equally close, prefer the one matching the distance
    the runner's own measured speed predicts.
3.  Only when two survivors match on both counts is the runner asked.

With no previous fix there is no speed ceiling at all: the app may have been
opened halfway through a race, where a stored zero says nothing.

-}
resolve : Route -> Maybe Time.Posix -> Progress -> Fix -> Maybe Resolution
resolve route startedAt progress fix =
    case candidates route fix.at of
        [] ->
            Nothing

        (nearest :: _) as all ->
            let
                pool =
                    if isPositionKnown progress then
                        narrow progress fix all

                    else
                        all

                best =
                    List.head pool |> Maybe.withDefault nearest

                tied =
                    List.filter (withinTie best) pool |> List.take 4
            in
            case NonEmpty.fromList tied of
                Nothing ->
                    Just (Resolved { progress = best, nearest = nearest })

                Just options ->
                    if NonEmpty.length options <= 1 then
                        Just (Resolved { progress = best, nearest = nearest })

                    else
                        Just (settleTie startedAt progress fix.taken options nearest)


isPositionKnown : Progress -> Bool
isPositionKnown progress =
    case Progress.lastFix progress of
        Just _ ->
            True

        Nothing ->
            Km.toFloat (Progress.km progress) > 0.05


narrow : Progress -> Fix -> List Candidate -> List Candidate
narrow progress fix all =
    let
        reached =
            Progress.km progress

        floorKm =
            Km.toFloat reached - Distance.inKilometers backwardTolerance

        elapsedHours =
            Basics.max 0
                (Duration.inHours (Duration.between (Progress.updatedAt progress) fix.taken))

        ceilingKm =
            Km.toFloat reached + (elapsedHours * maxSpeedKmPerHour) + 2

        strictFloorKm =
            case Progress.lastFix progress of
                Nothing ->
                    floorKm

                Just previous ->
                    let
                        crowFlies =
                            Distance.inKilometers (LatLon.haversine previous.at fix.at)
                    in
                    if crowFlies > 0.25 then
                        Basics.max floorKm (Km.toFloat reached + (crowFlies * 0.8))

                    else
                        floorKm

        within low high =
            List.filter
                (\(Candidate c) ->
                    Km.toFloat c.km >= low && Km.toFloat c.km <= high
                )
                all
    in
    firstNonEmpty
        [ within strictFloorKm ceilingKm
        , within floorKm ceilingKm
        , within floorKm (1 / 0)
        ]
        all


firstNonEmpty : List (List a) -> List a -> List a
firstNonEmpty attempts fallback =
    case attempts of
        [] ->
            fallback

        first :: rest ->
            if List.isEmpty first then
                firstNonEmpty rest fallback

            else
                first


withinTie : Candidate -> Candidate -> Bool
withinTie (Candidate best) (Candidate other) =
    Distance.inKilometers other.deviation
        - Distance.inKilometers best.deviation
        < Distance.inKilometers tieWindow


{-| The runner is only asked when the app genuinely cannot tell. A gap of more
than two kilometres between how well the options match the measured speed is
treated as decisive.
-}
settleTie : Maybe Time.Posix -> Progress -> Time.Posix -> NonEmpty Candidate -> Candidate -> Resolution
settleTie startedAt progress now options nearest =
    let
        reference =
            expectedKm startedAt progress now

        distanceFromReference (Candidate c) =
            abs (Km.toFloat c.km - reference)

        ranked =
            NonEmpty.sortBy distanceFromReference options

        chosen =
            NonEmpty.head ranked

        runnerUp =
            List.head (NonEmpty.tail ranked)
    in
    case runnerUp of
        Nothing ->
            Resolved { progress = chosen, nearest = nearest }

        Just second ->
            if distanceFromReference second - distanceFromReference chosen > 2 then
                Resolved { progress = chosen, nearest = nearest }

            else
                Ambiguous { options = ranked, nearest = nearest }


{-| Where the runner would be now if they kept the pace they have actually held.
Falls back to the last known milestone, which prefers the nearest option ahead.
-}
expectedKm : Maybe Time.Posix -> Progress -> Time.Posix -> Float
expectedKm startedAt progress now =
    let
        lastKnown =
            Km.toFloat (Progress.km progress)
    in
    case startedAt of
        Nothing ->
            lastKnown

        Just began ->
            case Progress.averageSpeedKmPerMinute began progress of
                Nothing ->
                    lastKnown

                Just speedKmPerMinute ->
                    let
                        idleMinutes =
                            Basics.max 0
                                (Duration.inMinutes
                                    (Duration.between (Progress.updatedAt progress) now)
                                )
                    in
                    lastKnown + (speedKmPerMinute * idleMinutes)


{-| Whether the runner has left the course, judged against the precision of the
measurement that says so.

A deviation of 80 m reported by a fix that is itself accurate only to 80 m is no
evidence at all. Sending someone 80 m in a chosen direction on that basis, in
the dark, is worse than saying nothing: hence the third state.

-}
routeState : Fix -> Candidate -> RouteState
routeState fix (Candidate c) =
    let
        deviation =
            Distance.inMeters c.deviation

        noise =
            Distance.inMeters fix.accuracy * 1.5

        threshold =
            Distance.inMeters offRouteThreshold
    in
    if deviation <= threshold then
        OnRoute

    else if deviation <= Basics.max threshold noise then
        Uncertain

    else
        OffRoute
