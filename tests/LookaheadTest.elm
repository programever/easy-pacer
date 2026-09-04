module LookaheadTest exposing (suite)

{-| The stretch of course ahead of the runner that carries direction arrows:
how far it reaches, and how the typed distance is read.
-}

import Core.App.Checkpoint as Checkpoint exposing (Cutoff(..))
import Core.App.Km as Km
import Core.App.Plan as Plan
import Core.App.Progress as Progress
import Core.App.Route as Route exposing (Route)
import Core.Data.Clock as Clock
import Core.Data.DateOnly as DateOnly
import Core.Data.Distance as Distance
import Expect
import State
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Lookahead"
        [ sliceSuite
        , typedSuite
        ]


{-| `lookahead` reads only `aheadText`; the rest of the race state is the
smallest plan that builds.
-}
typedSuite : Test
typedSuite =
    case Maybe.andThen (\route -> Result.toMaybe (Plan.fromDraft (draftOn route))) straightCourse of
        Nothing ->
            test "lookahead fixture plan builds" <| \_ -> Expect.fail "a start and a finish should make a plan"

        Just plan ->
            let
                metres typed =
                    Distance.inWholeMeters (State.lookahead (raceWith plan typed))
            in
            describe "The typed distance"
                [ test "a whole number of metres is taken as typed" <|
                    \_ -> Expect.equal (metres "800") 800
                , test "blank falls back to the default, not to nothing" <|
                    \_ -> Expect.equal (metres "") 500
                , test "nonsense falls back to the default" <|
                    \_ -> Expect.equal (metres "abc") 500
                , test "a negative number falls back to the default" <|
                    \_ -> Expect.equal (metres "-50") 500
                , test "zero is honoured: no arrows wanted" <|
                    \_ -> Expect.equal (metres "0") 0
                , test "the default text names the default distance" <|
                    \_ -> Expect.equal (metres State.defaultAheadText) 500
                ]


raceWith : Plan.Plan -> String -> State.RaceState
raceWith plan typed =
    { plan = plan
    , startedAt = Time.millisToPosix 0
    , progress = Progress.atStart (Time.millisToPosix 0)
    , tab = State.PlanTab
    , map = State.fitAll (Plan.route plan)
    , gesture = State.noGesture
    , scrub = State.NotScrubbing
    , kmEntryOpen = False
    , kmEntryText = ""
    , aheadText = typed
    , gpsPending = False
    }


draftOn : Route -> Plan.Draft
draftOn route =
    { route = Just route
    , checkpoints =
        [ Checkpoint.startLine (Checkpoint.idFromInt 0) Clock.midnight
        , Checkpoint.finishLine (Checkpoint.idFromInt 1) (Route.totalKm route) NoCutoff
        ]
    , date = DateOnly.fromParts 2026 9 4
    , time = Clock.fromString "06:00"
    , nextId = 2
    , name = ""
    }


{-| A straight course about a kilometre long, in four points, so a slice has
recorded points on both sides of it.
-}
straightCourse : Maybe Route
straightCourse =
    Route.fromSamples
        [ { lat = 10.5, lon = 107.1, ele = 0 }
        , { lat = 10.503, lon = 107.1, ele = 0 }
        , { lat = 10.506, lon = 107.1, ele = 0 }
        , { lat = 10.509, lon = 107.1, ele = 0 }
        ]
        []
        |> Result.toMaybe


sliceSuite : Test
sliceSuite =
    case straightCourse of
        Nothing ->
            test "slice fixture route builds" <| \_ -> Expect.fail "four points should make a route"

        Just route ->
            let
                -- Milestones of the slice, in whole hundreds of metres.
                kms from to =
                    Route.slice route (Km.fromFloat from) (Km.fromFloat to)
                        |> List.map (\point -> round (Km.toFloat point.km * 100))
            in
            describe "The stretch between two milestones"
                [ test "starts exactly at the first milestone and ends exactly at the second" <|
                    \_ ->
                        let
                            ends =
                                kms 0.2 0.5
                        in
                        Expect.equal ( List.head ends, List.head (List.reverse ends) ) ( Just 20, Just 50 )
                , test "keeps the recorded points that lie strictly between" <|
                    \_ ->
                        kms 0.2 0.5
                            |> List.length
                            |> Expect.equal 3
                , test "an end past the finish is clamped to the finish" <|
                    \_ ->
                        kms 0.9 5
                            |> List.reverse
                            |> List.head
                            |> Expect.equal (Just (round (Km.toFloat (Route.totalKm route) * 100)))
                , test "a zero length stretch is a single point, never a backwards line" <|
                    \_ ->
                        kms 0.5 0.5
                            |> List.length
                            |> Expect.equal 1
                ]
