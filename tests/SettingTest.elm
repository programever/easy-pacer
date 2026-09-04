module SettingTest exposing (suite)

{-| The setup screen as the runner meets it: one keystroke at a time.

These exist because the first build rewrote "053" to "00:53" under the
runner's finger and then blanked the box on the fourth digit, which made the
cutoff column look uneditable. Every test here types the way a person does.

-}

import Action exposing (SettingMsg(..))
import Core.App.Checkpoint as Checkpoint exposing (Cutoff(..), Role(..))
import Core.App.Km as Km
import Core.Data.Clock as Clock
import Expect
import Page.Setting as Setting
import State exposing (Field(..), Model, Screen(..), SetupState)
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Setting"
        [ describe "a cutoff typed one digit at a time"
            [ test "shows exactly what was typed while the box has focus" <|
                \_ ->
                    typed (CutoffOf station1) [ "0", "05", "053" ]
                        |> shownText (CutoffOf station1)
                        |> Expect.equal "053"
            , test "parses once all four digits are in" <|
                \_ ->
                    typed (CutoffOf station1) [ "0", "05", "053", "0530" ]
                        |> cutoffOf station1
                        |> Expect.equal (Just "05:30")
            , test "tidies to HH:MM when the box loses focus" <|
                \_ ->
                    typed (CutoffOf station1) [ "0", "05", "053", "0530" ]
                        |> step CommitTyping
                        |> shownText (CutoffOf station1)
                        |> Expect.equal "05:30"
            , test "clearing the box removes the cutoff" <|
                \_ ->
                    typed (CutoffOf station1) [ "0530", "" ]
                        |> step CommitTyping
                        |> cutoffOf station1
                        |> Expect.equal Nothing
            , test "nonsense is rejected on blur with a toast, not kept" <|
                \_ ->
                    let
                        after =
                            typed (CutoffOf station1) [ "abc" ] |> step CommitTyping
                    in
                    Expect.equal
                        ( cutoffOf station1 after, after.toast /= Nothing )
                        ( Nothing, True )
            ]
        , describe "the runner arranges the stations by hand"
            [ test "typing a km past the next station does not move the card" <|
                \_ ->
                    typed (KmOf station1) [ "9" ]
                        |> step CommitTyping
                        |> stationOrder
                        |> Expect.equal [ "A", "B" ]
            , test "the down button swaps a station with the one below" <|
                \_ ->
                    step (MoveStationDown station1) start
                        |> stationOrder
                        |> Expect.equal [ "B", "A" ]
            , test "the up button swaps a station with the one above" <|
                \_ ->
                    step (MoveStationUp station2) start
                        |> stationOrder
                        |> Expect.equal [ "B", "A" ]
            , test "the top station cannot move above the start" <|
                \_ ->
                    step (MoveStationUp station1) start
                        |> stationOrder
                        |> Expect.equal [ "A", "B" ]
            , test "the bottom station cannot move below the finish" <|
                \_ ->
                    step (MoveStationDown station2) start
                        |> stationOrder
                        |> Expect.equal [ "A", "B" ]
            , test "a new station joins at the end of the stations" <|
                \_ ->
                    step AddStation start
                        |> stationOrder
                        |> Expect.equal [ "A", "B", "" ]
            , test "the finish is still last after adding a station" <|
                \_ ->
                    step AddStation start
                        |> lastRole
                        |> Expect.equal (Just FinishLine)
            ]
        ]



-- FIXTURE


station1 : Checkpoint.Id
station1 =
    Checkpoint.idFromInt 1


station2 : Checkpoint.Id
station2 =
    Checkpoint.idFromInt 2


{-| Start, two stations at 3 km and 6 km, finish at 10 km. No route is
needed: nothing under test reads one.
-}
start : Model
start =
    let
        model =
            State.initialModel Time.utc (Time.millisToPosix 0)
    in
    { model
        | screen =
            Setting
                (State.setup
                    { route = Nothing
                    , checkpoints =
                        [ Checkpoint.startLine (Checkpoint.idFromInt 0) Clock.midnight
                        , Checkpoint.station station1 "A" (Km.fromFloat 3) NoCutoff
                        , Checkpoint.station station2 "B" (Km.fromFloat 6) NoCutoff
                        , Checkpoint.finishLine (Checkpoint.idFromInt 3) (Km.fromFloat 10) NoCutoff
                        ]
                    , date = Nothing
                    , time = Nothing
                    , nextId = 4
                    , name = ""
                    }
                )
    }


step : SettingMsg -> Model -> Model
step msg model =
    case model.screen of
        Setting state ->
            Tuple.first (Setting.update msg state model)

        Racing _ ->
            model


typed : Field -> List String -> Model
typed field keystrokes =
    List.foldl (\raw model -> step (Typed field raw) model) start keystrokes


setupState : Model -> Maybe SetupState
setupState model =
    case model.screen of
        Setting state ->
            Just state

        Racing _ ->
            Nothing


{-| What the box displays, by the same rule the view uses.
-}
shownText : Field -> Model -> String
shownText field model =
    case setupState model of
        Nothing ->
            "NOT ON SETUP SCREEN"

        Just state ->
            case state.typing of
                Just typing ->
                    if typing.field == field then
                        typing.text

                    else
                        parsedText field state

                Nothing ->
                    parsedText field state


parsedText : Field -> SetupState -> String
parsedText field state =
    case field of
        CutoffOf id ->
            Maybe.withDefault "" (cutoffOf id { start | screen = Setting state })

        _ ->
            ""


cutoffOf : Checkpoint.Id -> Model -> Maybe String
cutoffOf id model =
    setupState model
        |> Maybe.andThen
            (\state ->
                state.draft.checkpoints
                    |> List.filter (\checkpoint -> checkpoint.id == id)
                    |> List.head
            )
        |> Maybe.andThen Checkpoint.cutoffClock
        |> Maybe.map Clock.toString


lastRole : Model -> Maybe Role
lastRole model =
    setupState model
        |> Maybe.andThen (\state -> List.head (List.reverse state.draft.checkpoints))
        |> Maybe.map .role


stationOrder : Model -> List String
stationOrder model =
    setupState model
        |> Maybe.map
            (\state ->
                state.draft.checkpoints
                    |> List.filter (\checkpoint -> checkpoint.role == Station)
                    |> List.map .name
            )
        |> Maybe.withDefault []
