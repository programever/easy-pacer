module CoreTest exposing (suite)

{-| Each test is stated as the claim it proves. The fixtures are the real HBS
course and the real race sheet, so a regression shows up as a wrong number on a
real race rather than an abstract failure.
-}

import Core.App.Checkpoint as Checkpoint exposing (Cutoff(..))
import Core.App.Km as Km
import Core.App.LatLon exposing (LatLon)
import Core.App.Position as Position exposing (RouteState(..))
import Core.App.Route as Route
import Core.Data.Clock as Clock
import Core.Data.DateOnly as DateOnly
import Core.Data.Distance as Distance
import Expect
import State
import Storage.PlanFile as PlanFile
import Test exposing (Test, describe, test)
import Time


{-| The coordinate the app was tested against on the real course: a point on an
out-and-back section, matching both km 18.45 and km 22.86.
-}
onTheOverlap : LatLon
onTheOverlap =
    { lat = 10.537182, lon = 107.122646 }


suite : Test
suite =
    describe "Core"
        [ describe "Clock accepts what runners type"
            [ test "four digits" <|
                \_ -> Expect.equal (Maybe.map Clock.toString (Clock.fromString "0530")) (Just "05:30")
            , test "colon separated" <|
                \_ -> Expect.equal (Maybe.map Clock.toString (Clock.fromString "5:30")) (Just "05:30")
            , test "three digits" <|
                \_ -> Expect.equal (Maybe.map Clock.toString (Clock.fromString "530")) (Just "05:30")
            , test "h separated" <|
                \_ -> Expect.equal (Maybe.map Clock.toString (Clock.fromString "18h30")) (Just "18:30")
            , test "rejects an impossible hour" <|
                \_ -> Expect.equal (Clock.fromString "24:00") Nothing
            , test "rejects an impossible minute" <|
                \_ -> Expect.equal (Clock.fromString "5:75") Nothing
            ]
        , describe "DateOnly lets a race be set up days ahead"
            [ test "day and month, year assumed" <|
                \_ ->
                    Expect.equal
                        (Maybe.map DateOnly.toIsoString (DateOnly.fromString 2026 "18/8"))
                        (Just "2026-08-18")
            , test "rejects a day that does not exist" <|
                \_ -> Expect.equal (DateOnly.fromString 2026 "31/2") Nothing
            ]
        , describe "Checkpoint status follows distance in both directions"
            [ test "correcting distance downwards un-passes what is now ahead" <|
                \_ ->
                    let
                        moment =
                            Time.millisToPosix 1000

                        checkpoint =
                            Checkpoint.station (Checkpoint.idFromInt 1) "CP 1" (Km.fromFloat 8.5) NoCutoff

                        passed =
                            Checkpoint.syncStatus (Km.fromFloat 18.45) moment checkpoint

                        rewound =
                            Checkpoint.syncStatus Km.start moment passed
                    in
                    Expect.equal ( Checkpoint.isPassed passed, Checkpoint.isPending rewound )
                        ( True, True )
            ]
        , describe "Route state weighs the deviation against the accuracy"
            [ test "eighty metres out with eighty metres of noise proves nothing" <|
                \_ -> Expect.equal (stateFor 80 70) Uncertain
            , test "eighty metres out with a precise fix means off course" <|
                \_ -> Expect.equal (stateFor 80 10) OffRoute
            , test "close in is on course whatever the noise" <|
                \_ -> Expect.equal (stateFor 15 8) OnRoute
            , test "thirty metres out is still on course" <|
                \_ -> Expect.equal (stateFor 30 8) OnRoute
            , test "just past thirty metres with a precise fix means off course" <|
                \_ -> Expect.equal (stateFor 40 8) OffRoute
            ]
        , describe "Only a precise fix may move the milestone"
            [ test "eight metres of noise, on the course" <|
                \_ -> Expect.equal (trustFor 15 8) True
            , test "eighty metres of noise is refused even on the course" <|
                \_ -> Expect.equal (trustFor 15 80) False
            , test "a precise fix far from the course is refused too" <|
                \_ -> Expect.equal (trustFor 300 8) False
            ]
        , describe "A plan is saved under the name of the file it came from"
            [ test "gpx becomes json" <|
                \_ -> Expect.equal (PlanFile.nameFromFile "hbs-25k.gpx" ++ ".json") "hbs-25k.json"
            , test "dots inside the name survive" <|
                \_ -> Expect.equal (PlanFile.nameFromFile "HBS 2026.v2.gpx") "HBS 2026.v2"
            , test "no extension, no change" <|
                \_ -> Expect.equal (PlanFile.nameFromFile "route") "route"
            ]
        , gestureSuite
        ]


trustFor : Float -> Float -> Bool
trustFor deviationMetres accuracyMetres =
    Position.isTrustworthy
        { at = onTheOverlap, accuracy = Distance.fromMeters accuracyMetres, taken = Time.millisToPosix 0 }
        (Position.candidateAt (Km.fromFloat 18.45) (Distance.fromMeters deviationMetres) onTheOverlap)


{-| Builds a candidate at a chosen deviation without needing a real course.
-}
stateFor : Float -> Float -> RouteState
stateFor deviationMetres accuracyMetres =
    let
        fix =
            { at = onTheOverlap
            , accuracy = Distance.fromMeters accuracyMetres
            , taken = Time.millisToPosix 0
            }
    in
    Position.routeState fix
        (Position.candidateAt
            (Km.fromFloat 18.45)
            (Distance.fromMeters deviationMetres)
            onTheOverlap
        )


{-| Map gestures, with the browser out of the picture. The route is a straight
1 km line: enough for scale clamping to have something to clamp to.
-}
gestureSuite : Test
gestureSuite =
    case
        Route.fromSamples
            [ { lat = 10.5, lon = 107.1, ele = 0 }
            , { lat = 10.509, lon = 107.1, ele = 0 }
            ]
            []
    of
        Err _ ->
            test "gesture fixture route builds" <| \_ -> Expect.fail "two points should make a route"

        Ok route ->
            let
                view =
                    { centre = { x = 0, y = 0 }, scale = 100 }

                oneFinger =
                    State.pointerDown 1 { x = 10, y = 10 } view State.noGesture
            in
            describe "Map gestures"
                [ test "one finger dragging right pans the centre left" <|
                    \_ ->
                        State.pointerMove route 1 1 { x = 30, y = 10 } ( oneFinger, view )
                            |> Tuple.second
                            |> .centre
                            |> .x
                            |> Expect.within (Expect.Absolute 0.0001) -0.2
                , test "a finger that was never pressed on the map is ignored" <|
                    \_ ->
                        State.pointerMove route 1 7 { x = 30, y = 10 } ( oneFinger, view )
                            |> Tuple.second
                            |> Expect.equal view
                , test "two fingers spreading apart zoom in" <|
                    \_ ->
                        let
                            twoFingers =
                                State.pointerDown 2 { x = 110, y = 10 } view oneFinger
                        in
                        State.pointerMove route 1 2 { x = 210, y = 10 } ( twoFingers, view )
                            |> Tuple.second
                            |> .scale
                            |> Expect.within (Expect.Absolute 0.0001) 200
                , test "lifting one of two fingers ends the pinch" <|
                    \_ ->
                        State.pointerDown 2 { x = 110, y = 10 } view oneFinger
                            |> State.pointerUp 2
                            |> .pinch
                            |> Expect.equal Nothing
                ]
