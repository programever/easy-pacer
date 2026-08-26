module CoreTest exposing (suite)

{-| Each test is stated as the claim it proves. The fixtures are the real HBS
course and the real race sheet, so a regression shows up as a wrong number on a
real race rather than an abstract failure.
-}

import Core.App.Checkpoint as Checkpoint exposing (Cutoff(..))
import Core.App.Km as Km
import Core.App.LatLon exposing (LatLon)
import Core.App.Plan as Plan
import Core.App.Position as Position exposing (RouteState(..))
import Core.App.Route as Route
import Core.App.Segment as Segment
import Core.Data.Clock as Clock
import Core.Data.DateOnly as DateOnly
import Core.Data.Distance as Distance
import Core.Data.Duration as Duration
import Core.Data.Elevation as Elevation
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
        , describe "An edit box gives back the km it was given"
            [ test "keeps three decimals" <|
                \_ -> Expect.equal (Km.toEditString (Km.fromFloat 8.555)) "8.555"
            , test "keeps two" <|
                \_ -> Expect.equal (Km.toEditString (Km.fromFloat 8.55)) "8.55"
            , test "keeps one" <|
                \_ -> Expect.equal (Km.toEditString (Km.fromFloat 8.5)) "8.5"
            , test "keeps a zero that carries a digit" <|
                \_ -> Expect.equal (Km.toEditString (Km.fromFloat 8.05)) "8.05"
            , test "drops the decimal point from a whole number" <|
                \_ -> Expect.equal (Km.toEditString (Km.fromFloat 40)) "40"
            , test "trims a measured value to the metre" <|
                \_ -> Expect.equal (Km.toEditString (Km.fromFloat 8.441149565280835)) "8.441"
            , test "the label still rounds, which is why the two differ" <|
                \_ -> Expect.equal (Km.toString (Km.fromFloat 8.555)) "8.6"
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
        , describe "The cutoff colour is the pace the budget demands"
            -- 5 km with 300 m of climb is 8 flat-equivalent km at 100 m = 1000 m.
            [ test "over 15 minutes per km to spare is green" <|
                \_ -> Expect.equal (paceUrgency 130 5 300) Segment.Comfortable
            , test "exactly 15 is already yellow" <|
                \_ -> Expect.equal (paceUrgency 120 5 300) Segment.Tight
            , test "exactly 10 is still yellow" <|
                \_ -> Expect.equal (paceUrgency 80 5 300) Segment.Tight
            , test "under 10 minutes per km is red" <|
                \_ -> Expect.equal (paceUrgency 79 5 300) Segment.Critical
            , test "a cutoff already behind is missed" <|
                \_ -> Expect.equal (paceUrgency -5 5 300) Segment.Missed
            , test "the climb ratio changes the verdict" <|
                -- Same leg at 100 m = 2000 m: 11 flat km, 120 minutes is under 11 min/km.
                \_ ->
                    Expect.equal
                        (Segment.urgency 2000 (segmentFor 120 5 300))
                        Segment.Tight
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
        , toastSuite
        , targetSuite
        , overnightSuite
        ]


{-| A toast is set by whatever raised it and never dismissed by hand, so the
proposition is that the clock alone takes it away, and not before its time.
-}
toastSuite : Test
toastSuite =
    let
        shown =
            let
                blank =
                    State.initialModel Time.utc (Time.millisToPosix 0)
            in
            { blank | toast = Just { text = "toast", shownAt = Time.millisToPosix 0 } }
    in
    describe "A toast leaves on its own after thirty seconds"
        [ test "a second short of its time it is still on screen" <|
            \_ ->
                State.tick (Time.millisToPosix 29000) shown
                    |> .toast
                    |> Expect.notEqual Nothing
        , test "at thirty seconds the tick clears it" <|
            \_ ->
                State.tick (Time.millisToPosix 30000) shown
                    |> .toast
                    |> Expect.equal Nothing
        , test "a tick with no toast showing leaves the model alone" <|
            \_ ->
                let
                    quiet =
                        State.initialModel Time.utc (Time.millisToPosix 0)
                in
                State.tick (Time.millisToPosix 90000) quiet
                    |> Expect.equal { quiet | now = Time.millisToPosix 90000 }
        ]


{-| The plan review rejects targets that run backwards along the course.
-}
targetSuite : Test
targetSuite =
    case
        Route.fromSamples
            [ { lat = 10.5, lon = 107.1, ele = 0 }
            , { lat = 10.509, lon = 107.1, ele = 0 }
            ]
            []
    of
        Err _ ->
            test "target fixture route builds" <| \_ -> Expect.fail "two points should make a route"

        Ok course ->
            let
                planWith firstTarget secondTarget =
                    Plan.fromDraft
                        { route = Just course
                        , checkpoints =
                            [ Checkpoint.startLine (Checkpoint.idFromInt 0) gunTime
                            , withTarget firstTarget (Checkpoint.station (Checkpoint.idFromInt 1) "A" (Km.fromFloat 0.3) NoCutoff)
                            , withTarget secondTarget (Checkpoint.station (Checkpoint.idFromInt 2) "B" (Km.fromFloat 0.6) NoCutoff)
                            , Checkpoint.finishLine (Checkpoint.idFromInt 3) (Km.fromFloat 0.9) NoCutoff
                            ]
                        , date = DateOnly.fromParts 2026 8 25
                        , time = Clock.fromString "05:30"
                        , nextId = 4
                        , name = ""
                        , climbRatio = 1000
                        }

                targetIssues result =
                    case result of
                        Err _ ->
                            [ "PLAN DID NOT BUILD" ]

                        Ok plan ->
                            Plan.issues Time.utc plan
                                |> List.map Plan.issueText
                                |> List.filter (String.contains "Mục tiêu")

                cutoffPlan target =
                    Plan.fromDraft
                        { route = Just course
                        , checkpoints =
                            [ Checkpoint.startLine (Checkpoint.idFromInt 0) gunTime
                            , withTarget target
                                (Checkpoint.station (Checkpoint.idFromInt 1)
                                    "A"
                                    (Km.fromFloat 0.3)
                                    (Maybe.map ClosesAt (Clock.fromString "09:00") |> Maybe.withDefault NoCutoff)
                                )
                            , Checkpoint.finishLine (Checkpoint.idFromInt 2) (Km.fromFloat 0.9) NoCutoff
                            ]
                        , date = DateOnly.fromParts 2026 8 25
                        , time = Clock.fromString "05:30"
                        , nextId = 3
                        , name = ""
                        , climbRatio = 1000
                        }
            in
            describe "Targets must advance with the course"
                [ test "a later checkpoint with an earlier target is flagged" <|
                    \_ ->
                        targetIssues (planWith "08:00" "07:30")
                            |> List.length
                            |> Expect.equal 1
                , test "targets in course order pass" <|
                    \_ ->
                        targetIssues (planWith "07:30" "08:00")
                            |> Expect.equal []
                , test "a target after the station's own COT is flagged" <|
                    \_ ->
                        targetIssues (cutoffPlan "09:30")
                            |> List.length
                            |> Expect.equal 1
                , test "a target exactly at the COT is allowed" <|
                    \_ ->
                        targetIssues (cutoffPlan "09:00")
                            |> Expect.equal []
                , test "a station cannot aim later than the finish" <|
                    \_ ->
                        targetIssues
                            (Plan.fromDraft
                                { route = Just course
                                , checkpoints =
                                    [ Checkpoint.startLine (Checkpoint.idFromInt 0) gunTime
                                    , withTarget "11:30" (Checkpoint.station (Checkpoint.idFromInt 1) "A" (Km.fromFloat 0.3) NoCutoff)
                                    , withTarget "11:00" (Checkpoint.finishLine (Checkpoint.idFromInt 2) (Km.fromFloat 0.9) NoCutoff)
                                    ]
                                , date = DateOnly.fromParts 2026 8 25
                                , time = Clock.fromString "05:30"
                                , nextId = 3
                                , name = ""
                                , climbRatio = 1000
                                }
                            )
                            |> List.length
                            |> Expect.equal 1
                ]


withTarget : String -> Checkpoint.Checkpoint -> Checkpoint.Checkpoint
withTarget clockText checkpoint =
    { checkpoint | target = Clock.fromString clockText }


gunTime : Clock.Clock
gunTime =
    clockAt "05:30"


clockAt : String -> Clock.Clock
clockAt text =
    Clock.fromString text |> Maybe.withDefault Clock.midnight


{-| Races that cross midnight, or run into a second day. Each cutoff is
anchored to the one before it, so the sheet stays in order however many
midnights pass under it.
-}
overnightSuite : Test
overnightSuite =
    case
        Route.fromSamples
            [ { lat = 10.5, lon = 107.1, ele = 0 }
            , { lat = 10.509, lon = 107.1, ele = 0 }
            ]
            []
    of
        Err _ ->
            test "overnight fixture route builds" <| \_ -> Expect.fail "two points should make a route"

        Ok course ->
            let
                planFrom startText stations =
                    Plan.fromDraft
                        { route = Just course
                        , checkpoints =
                            Checkpoint.startLine (Checkpoint.idFromInt 0) (clockAt startText)
                                :: stations
                        , date = DateOnly.fromParts 2026 8 25
                        , time = Clock.fromString startText
                        , nextId = 9
                        , name = ""
                        , climbRatio = 1000
                        }

                orderIssues result =
                    case result of
                        Err _ ->
                            [ "PLAN DID NOT BUILD" ]

                        Ok plan ->
                            Plan.issues Time.utc plan
                                |> List.map Plan.issueText
                                |> List.filter (String.contains "không nằm sau")
            in
            describe "Cutoffs follow the race across midnight"
                [ test "a 22:00 start with a 02:00 cutoff stays in order" <|
                    \_ ->
                        orderIssues
                            (planFrom "22:00"
                                [ Checkpoint.station (Checkpoint.idFromInt 1) "A" (Km.fromFloat 0.3) (ClosesAt (clockAt "23:30"))
                                , Checkpoint.finishLine (Checkpoint.idFromInt 2) (Km.fromFloat 0.9) (ClosesAt (clockAt "02:00"))
                                ]
                            )
                            |> Expect.equal []
                , test "a cutoff on the second morning stays in order" <|
                    \_ ->
                        orderIssues
                            (planFrom "05:30"
                                [ Checkpoint.station (Checkpoint.idFromInt 1) "A" (Km.fromFloat 0.3) (ClosesAt (clockAt "20:00"))
                                , Checkpoint.finishLine (Checkpoint.idFromInt 2) (Km.fromFloat 0.9) (ClosesAt (clockAt "08:00"))
                                ]
                            )
                            |> Expect.equal []
                , test "a checkpoint a full day after the start is valid" <|
                    \_ ->
                        orderIssues
                            (planFrom "22:00"
                                [ Checkpoint.station (Checkpoint.idFromInt 1) "A" (Km.fromFloat 0.3) (ClosesAt (clockAt "23:00"))
                                , Checkpoint.finishLine (Checkpoint.idFromInt 2) (Km.fromFloat 0.9) (ClosesAt (clockAt "22:00"))
                                ]
                            )
                            |> Expect.equal []
                , test "a stale start cutoff cannot poison the anchor" <|
                    -- The draft's start row says 00:00 because the course was
                    -- loaded before the start time was typed; the plan pins it.
                    \_ ->
                        (case
                            Plan.fromDraft
                                { route = Just course
                                , checkpoints =
                                    [ Checkpoint.startLine (Checkpoint.idFromInt 0) (clockAt "00:00")
                                    , Checkpoint.station (Checkpoint.idFromInt 1) "A" (Km.fromFloat 0.3) (ClosesAt (clockAt "10:30"))
                                    , Checkpoint.finishLine (Checkpoint.idFromInt 2) (Km.fromFloat 0.9) (ClosesAt (clockAt "14:00"))
                                    ]
                                , date = DateOnly.fromParts 2026 8 29
                                , time = Clock.fromString "02:00"
                                , nextId = 9
                                , name = ""
                                , climbRatio = 1000
                                }
                         of
                            Err _ ->
                                [ "PLAN DID NOT BUILD" ]

                            Ok plan ->
                                Plan.issues Time.utc plan
                                    |> List.filter Plan.isBlocking
                                    |> List.map Plan.issueText
                        )
                            |> Expect.equal []
                , test "targets ride the same day as their cutoffs" <|
                    \_ ->
                        (case
                            planFrom "22:00"
                                [ withTarget "23:00" (Checkpoint.station (Checkpoint.idFromInt 1) "A" (Km.fromFloat 0.3) (ClosesAt (clockAt "23:30")))
                                , withTarget "01:30" (Checkpoint.finishLine (Checkpoint.idFromInt 2) (Km.fromFloat 0.9) (ClosesAt (clockAt "02:00")))
                                ]
                         of
                            Err _ ->
                                [ "PLAN DID NOT BUILD" ]

                            Ok plan ->
                                Plan.issues Time.utc plan
                                    |> List.map Plan.issueText
                                    |> List.filter (String.contains "Mục tiêu")
                        )
                            |> Expect.equal []
                ]


paceUrgency : Float -> Float -> Float -> Segment.Urgency
paceUrgency minutesLeft km ascentMetres =
    Segment.urgency 1000 (segmentFor minutesLeft km ascentMetres)


segmentFor : Float -> Float -> Float -> Segment.Segment
segmentFor minutesLeft km ascentMetres =
    { distance = Distance.fromKilometers km
    , ascent = Elevation.fromMeters ascentMetres
    , descent = Elevation.fromMeters 0
    , cutoff =
        Just
            { closesAt = Time.millisToPosix 0
            , remaining = Duration.fromMinutes minutesLeft
            }
    }


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
