module SnapshotTest exposing (suite)

{-| A refresh in the middle of a race must put the runner back exactly where
they were. The proposition: capture, encode, decode, restore is the identity on
everything the race screen shows.
-}

import Core.App.Checkpoint as Checkpoint exposing (Cutoff(..))
import Core.App.Km as Km
import Core.App.Plan as Plan
import Core.App.Progress as Progress
import Core.App.Route as Route
import Core.Data.Clock as Clock
import Core.Data.DateOnly as DateOnly
import Core.Data.Distance as Distance
import Core.Data.NonEmpty as NonEmpty
import Expect
import Json.Decode as Decode
import State exposing (Model, Screen(..))
import Storage.Snapshot as Snapshot
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Snapshot"
        [ test "a race survives the round trip" <|
            \_ ->
                case racing of
                    Nothing ->
                        Expect.fail "fixture did not build"

                    Just model ->
                        Expect.equal (summary (roundTrip model)) (summary model)
        , test "a plan still being set up survives too, without inventing a race" <|
            \_ ->
                case racing of
                    Nothing ->
                        Expect.fail "fixture did not build"

                    Just model ->
                        let
                            settingUp =
                                { model | screen = Setting (State.setup (Snapshot.capture model).draft) }
                        in
                        Expect.equal (summary (roundTrip settingUp)) (summary settingUp)
        , test "nothing is saved for a message that changes nothing worth saving" <|
            \_ ->
                case racing of
                    Nothing ->
                        Expect.fail "fixture did not build"

                    Just model ->
                        Expect.equal (Snapshot.capture { model | now = Time.millisToPosix 99 })
                            (Snapshot.capture model)
        ]


roundTrip : Model -> Model
roundTrip model =
    let
        blank =
            State.initialModel Time.utc (Time.millisToPosix 0)
    in
    case Decode.decodeValue Snapshot.decoder (Snapshot.encode (Snapshot.capture model)) of
        Ok snapshot ->
            Snapshot.restore snapshot blank

        Err _ ->
            blank


{-| Everything the race screen reads, as plain data that can be compared.
-}
summary : Model -> List String
summary model =
    case model.screen of
        Setting state ->
            "setup"
                :: List.map (\c -> c.name ++ "@" ++ Km.toString c.km) state.draft.checkpoints
                ++ [ model.sosPhone ]

        Racing race ->
            [ "racing"
            , "started " ++ String.fromInt (Time.posixToMillis race.startedAt)
            , "km " ++ Km.toString (Progress.km race.progress)
            , "updated " ++ String.fromInt (Time.posixToMillis (Progress.updatedAt race.progress))
            , "fix "
                ++ (Progress.lastFix race.progress
                        |> Maybe.map (\f -> String.fromFloat f.at.lat)
                        |> Maybe.withDefault "none"
                   )
            , "crumbs " ++ String.fromInt (List.length (Progress.breadcrumbs race.progress))
            , "phone " ++ model.sosPhone
            ]
                ++ List.map
                    (\c ->
                        c.name
                            ++ " "
                            ++ (case Checkpoint.passedAt c of
                                    Just at ->
                                        "passed@" ++ String.fromInt (Time.posixToMillis at)

                                    Nothing ->
                                        "pending"
                               )
                    )
                    (NonEmpty.toList (Plan.checkpoints race.plan))


{-| Two checkpoints passed, one ahead, a GPS fix on record.
-}
racing : Maybe Model
racing =
    let
        draft =
            { route =
                Route.fromSamples
                    [ { lat = 10.5, lon = 107.1, ele = 10 }
                    , { lat = 10.509, lon = 107.1, ele = 60 }
                    , { lat = 10.518, lon = 107.1, ele = 20 }
                    , { lat = 10.527, lon = 107.1, ele = 90 }
                    ]
                    []
                    |> Result.toMaybe
            , checkpoints =
                [ Checkpoint.startLine (Checkpoint.idFromInt 0) Clock.midnight
                , Checkpoint.station (Checkpoint.idFromInt 1) "A" (Km.fromFloat 1) NoCutoff
                , Checkpoint.station (Checkpoint.idFromInt 2) "B" (Km.fromFloat 2) (Maybe.map ClosesAt (Clock.fromString "08:00") |> Maybe.withDefault NoCutoff)
                , Checkpoint.finishLine (Checkpoint.idFromInt 3) (Km.fromFloat 3) NoCutoff
                ]
            , date = DateOnly.fromParts 2026 7 25
            , time = Clock.fromString "06:00"
            , nextId = 4
            , name = ""
            , climbRatio = 1000
            }

        fix =
            { at = { lat = 10.51, lon = 107.1 }, accuracy = Distance.fromMeters 8, taken = Time.millisToPosix 5000 }

        progress =
            Progress.atStart (Time.millisToPosix 1000)
                |> Progress.fromGps (Km.fromFloat 1.5) fix True
    in
    Plan.fromDraft draft
        |> Result.toMaybe
        |> Maybe.map
            (\plan ->
                let
                    blank =
                        State.initialModel Time.utc (Time.millisToPosix 0)
                in
                { blank
                    | sosPhone = "0901234567"
                    , screen =
                        Racing
                            { plan =
                                Plan.withCheckpoints
                                    (NonEmpty.map (Checkpoint.syncStatus (Km.fromFloat 1.5) (Time.millisToPosix 5000)) (Plan.checkpoints plan))
                                    plan
                            , startedAt = Time.millisToPosix 1000
                            , progress = progress
                            , tab = State.PlanTab
                            , map = State.fitAll (Plan.route plan)
                            , gesture = State.noGesture
                            , scrub = State.NotScrubbing
                            , kmEntryOpen = False
                            , kmEntryText = ""
                            , gpsPending = False
                            }
                }
            )
