module Page.Setting exposing (update, view)

{-| Building the race sheet: load the course, say when you start, list the
checkpoints and their cutoffs.

Everything here edits a `Draft`. A `Draft` cannot be run: turning one into a
`Plan` goes through `Plan.fromDraft`, which is also where the review lives.

Stations stay in the order the runner arranges them: the up and down buttons
on a card move it one place, and the start and finish are pinned to the ends.
The only automatic km sort is seeding stations from the GPX file's waypoints —
those arrive unordered and the runner has not arranged anything yet.

-}

import Action exposing (Msg(..), SettingMsg(..))
import Core.App.Checkpoint as Checkpoint exposing (Checkpoint, Cutoff(..), Role(..))
import Core.App.Km as Km
import Core.App.Plan as Plan exposing (Draft, Issue)
import Core.App.Position as Position
import Core.App.Progress as Progress
import Core.App.Route as Route
import Core.Data.Clock as Clock
import Core.Data.DateOnly as DateOnly
import Core.Data.Elevation as Elevation
import Core.Data.NonEmpty as NonEmpty
import File
import File.Download
import File.Select
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Runtime.Ports as Ports
import State exposing (Dialog(..), Field(..), Model, Screen(..), Scrub(..), SetupState)
import Storage.PlanFile as PlanFile
import Storage.Seeded as Seeded
import Task
import Time
import View.Form as Form
import View.Pointer as Pointer
import View.Profile as Profile
import View.Theme as Theme



-- UPDATE


update : SettingMsg -> SetupState -> Model -> ( Model, Cmd Msg )
update msg state model =
    let
        draft =
            state.draft
    in
    case msg of
        PickGpxFile ->
            ( model, File.Select.file [] (SettingChanged << GpxFileSelected) )

        GpxFileSelected file ->
            ( withDraft { draft | name = PlanFile.nameFromFile (File.name file) } state model
            , Task.perform (SettingChanged << GpxTextRead) (File.toString file)
            )

        GpxTextRead contents ->
            ( model, Ports.parseGpx contents )

        SeedFromWaypoints ->
            ( withDraft (seedFromWaypoints draft) state model, Cmd.none )

        SeedPlanChosen label ->
            case List.filter (\plan -> plan.label == label) Seeded.plans of
                [] ->
                    ( model, Cmd.none )

                plan :: _ ->
                    case Decode.decodeString PlanFile.decoder plan.json of
                        Ok loaded ->
                            ( withDraft { loaded | checkpoints = pinEnds loaded.checkpoints } state model
                            , Cmd.none
                            )

                        Err _ ->
                            ( toast "Kế hoạch có sẵn bị lỗi — báo giúp bọn mình trên GitHub nhé." model, Cmd.none )

        AddStation ->
            let
                ( finishes, upToFinish ) =
                    List.partition (\checkpoint -> checkpoint.role == FinishLine) draft.checkpoints
            in
            ( withDraft
                { draft
                    | checkpoints =
                        upToFinish
                            ++ [ Checkpoint.station
                                    (Checkpoint.idFromInt draft.nextId)
                                    ""
                                    Km.start
                                    NoCutoff
                               ]
                            ++ finishes
                    , nextId = draft.nextId + 1
                }
                state
                model
            , Cmd.none
            )

        RemoveStation id ->
            ( withDraft
                { draft
                    | checkpoints =
                        List.filter (\checkpoint -> checkpoint.id /= id) draft.checkpoints
                }
                state
                model
            , Cmd.none
            )

        MoveStationUp id ->
            ( withDraft
                { draft | checkpoints = Checkpoint.moveUp id draft.checkpoints }
                state
                model
            , Cmd.none
            )

        MoveStationDown id ->
            ( withDraft
                { draft | checkpoints = Checkpoint.moveDown id draft.checkpoints }
                state
                model
            , Cmd.none
            )

        EditName id name ->
            ( withDraft
                { draft | checkpoints = List.map (renameIf id name) draft.checkpoints }
                state
                model
            , Cmd.none
            )

        Typed field raw ->
            let
                ( parsed, valid ) =
                    applyTyped model field raw draft
            in
            ( withSetup
                { state
                    | draft = parsed
                    , typing = Just { field = field, text = raw, valid = valid }
                }
                model
            , Cmd.none
            )

        CommitTyping ->
            let
                committed =
                    withSetup { state | typing = Nothing } model
            in
            case state.typing of
                Just typing ->
                    if typing.valid then
                        ( committed, Cmd.none )

                    else
                        ( toast (rejectedText typing.field) committed, Cmd.none )

                Nothing ->
                    ( committed, Cmd.none )

        UseToday ->
            ( withDraft { draft | date = Just (DateOnly.fromPosix model.zone model.now) } state model
            , Cmd.none
            )

        UseTomorrow ->
            ( withDraft
                { draft | date = Just (DateOnly.addDays 1 (DateOnly.fromPosix model.zone model.now)) }
                state
                model
            , Cmd.none
            )

        UseCurrentTime ->
            ( withDraft { draft | time = Just (Clock.fromPosix model.zone model.now) } state model
            , Cmd.none
            )

        ScrubSetup offsetX elementWidth ->
            case draft.route of
                Nothing ->
                    ( model, Cmd.none )

                Just course ->
                    ( withSetup
                        { state
                            | scrub =
                                ScrubbingAt
                                    (Profile.kmAtFraction course (offsetX / Basics.max 1 elementWidth))
                        }
                        model
                    , Cmd.none
                    )

        ReviewPlan ->
            ( { model | dialog = Just (PlanReview (reviewIssues model draft) False) }, Cmd.none )

        StartRace ->
            case reviewIssues model draft of
                [] ->
                    startRace draft model

                found ->
                    ( { model | dialog = Just (PlanReview found True) }, Cmd.none )

        ConfirmStartRace ->
            startRace draft { model | dialog = Nothing }

        ExportPlan ->
            ( model
            , File.Download.string (PlanFile.filename draft)
                "application/json"
                (Encode.encode 0 (PlanFile.encode draft))
            )

        PickPlanFile ->
            ( model, File.Select.file [] (SettingChanged << PlanFileSelected) )

        PlanFileSelected file ->
            ( withDraft { draft | name = PlanFile.nameFromFile (File.name file) } state model
            , Task.perform (SettingChanged << PlanTextRead) (File.toString file)
            )

        PlanTextRead contents ->
            case Decode.decodeString PlanFile.decoder contents of
                Ok loaded ->
                    ( withDraft { loaded | checkpoints = pinEnds loaded.checkpoints, name = draft.name } state model
                    , Cmd.none
                    )

                Err _ ->
                    ( toast "File kế hoạch không đọc được." model, Cmd.none )


currentYear : Model -> Int
currentYear model =
    Time.toYear model.zone model.now


withSetup : SetupState -> Model -> Model
withSetup state model =
    { model | screen = Setting state }


withDraft : Draft -> SetupState -> Model -> Model
withDraft draft state model =
    withSetup { state | draft = draft } model


toast : String -> Model -> Model
toast content model =
    { model | toast = Just { text = content, shownAt = model.now } }


{-| Parse what was typed into the draft. The second result says whether the
text was acceptable, which decides whether blur complains. Blank is always
acceptable: clearing a box is how a cutoff is removed.
-}
applyTyped : Model -> Field -> String -> Draft -> ( Draft, Bool )
applyTyped model field raw draft =
    let
        blank =
            String.trim raw == ""
    in
    case field of
        DateField ->
            let
                parsed =
                    DateOnly.fromString (currentYear model) raw
            in
            ( { draft | date = parsed }, blank || parsed /= Nothing )

        TimeField ->
            let
                parsed =
                    Clock.fromString raw
            in
            ( { draft | time = parsed }, blank || parsed /= Nothing )

        KmOf id ->
            case ( blank, String.toFloat raw ) of
                ( True, _ ) ->
                    ( { draft | checkpoints = List.map (setKmIf id Km.start) draft.checkpoints }, True )

                ( False, Just value ) ->
                    ( { draft | checkpoints = List.map (setKmIf id (Km.fromFloat value)) draft.checkpoints }
                    , True
                    )

                ( False, Nothing ) ->
                    ( draft, False )

        CutoffOf id ->
            let
                parsed =
                    Clock.fromString raw

                cutoff =
                    Maybe.map ClosesAt parsed |> Maybe.withDefault NoCutoff
            in
            ( { draft | checkpoints = List.map (setCutoffIf id cutoff) draft.checkpoints }
            , blank || parsed /= Nothing
            )

        TargetOf id ->
            let
                parsed =
                    Clock.fromString raw
            in
            ( { draft | checkpoints = List.map (setTargetIf id parsed) draft.checkpoints }
            , blank || parsed /= Nothing
            )

        ClimbField ->
            case ( blank, String.toInt raw ) of
                ( True, _ ) ->
                    ( { draft | climbRatio = 1000 }, True )

                ( False, Just value ) ->
                    if value > 0 then
                        ( { draft | climbRatio = value }, True )

                    else
                        ( draft, False )

                ( False, Nothing ) ->
                    ( draft, False )


renameIf : Checkpoint.Id -> String -> Checkpoint -> Checkpoint
renameIf id name checkpoint =
    if checkpoint.id == id then
        { checkpoint | name = name }

    else
        checkpoint


setKmIf : Checkpoint.Id -> Km.Km -> Checkpoint -> Checkpoint
setKmIf id km checkpoint =
    if checkpoint.id == id && checkpoint.role == Station then
        { checkpoint | km = km }

    else
        checkpoint


setCutoffIf : Checkpoint.Id -> Cutoff -> Checkpoint -> Checkpoint
setCutoffIf id cutoff checkpoint =
    if checkpoint.id == id then
        { checkpoint | cutoff = cutoff }

    else
        checkpoint


setTargetIf : Checkpoint.Id -> Maybe Clock.Clock -> Checkpoint -> Checkpoint
setTargetIf id target checkpoint =
    if checkpoint.id == id then
        { checkpoint | target = target }

    else
        checkpoint


{-| User facing. What blur says when the box held something that was not a
value. Matches the placeholder, so the fix is on screen already.
-}
rejectedText : Field -> String
rejectedText field =
    case field of
        DateField ->
            "Ngày chưa đúng — nhập kiểu 18/08/2026."

        TimeField ->
            "Giờ chưa đúng — nhập kiểu 05:30."

        KmOf _ ->
            "Số km chưa đúng — nhập một con số, ví dụ 8.5."

        CutoffOf _ ->
            "Giờ chưa đúng — nhập kiểu 05:30."

        TargetOf _ ->
            "Giờ chưa đúng — nhập kiểu 05:30."

        ClimbField ->
            "Số mét chưa đúng — nhập số nguyên dương, ví dụ 1000."


{-| Start first, then the stations in the order given, then the finish. The
stations are never sorted here: their order belongs to the runner.
-}
pinEnds : List Checkpoint -> List Checkpoint
pinEnds checkpoints =
    let
        withRole role =
            List.filter (\checkpoint -> checkpoint.role == role) checkpoints
    in
    withRole StartLine ++ withRole Station ++ withRole FinishLine


reviewIssues : Model -> Draft -> List Issue
reviewIssues model draft =
    case Plan.fromDraft draft of
        Err problems ->
            NonEmpty.toList problems

        Ok plan ->
            Plan.issues model.zone plan


startRace : Draft -> Model -> ( Model, Cmd Msg )
startRace draft model =
    case Plan.fromDraft draft of
        Err problems ->
            ( { model | dialog = Just (PlanReview (NonEmpty.toList problems) False) }, Cmd.none )

        Ok plan ->
            let
                began =
                    Plan.startsAt model.zone plan
            in
            ( { model
                | screen =
                    Racing
                        { plan = plan
                        , startedAt = began
                        , progress = Progress.atStart began
                        , tab = State.PlanTab
                        , map = State.fitAll (Plan.route plan)
                        , gesture = State.noGesture
                        , scrub = NotScrubbing
                        , kmEntryOpen = False
                        , kmEntryText = ""
                        , gpsPending = False
                        }
                , dialog = Nothing
              }
            , Cmd.none
            )


{-| Waypoints in a GPX file are snapped onto the course to get their milestone,
then anything at the very start or end is dropped: those are the start and
finish, which already exist.
-}
seedFromWaypoints : Draft -> Draft
seedFromWaypoints draft =
    case draft.route of
        Nothing ->
            draft

        Just course ->
            let
                courseLength =
                    Km.toFloat (Route.totalKm course)

                seeded =
                    Route.waypoints course
                        |> List.filterMap
                            (\waypoint ->
                                Position.candidates course waypoint.position
                                    |> List.head
                                    |> Maybe.map
                                        (\candidate ->
                                            ( waypoint.name, Position.candidateKm candidate )
                                        )
                            )
                        |> List.filter
                            (\( _, km ) ->
                                Km.toFloat km > 0.15 && Km.toFloat km < courseLength - 0.15
                            )
                        |> List.indexedMap
                            (\index ( name, km ) ->
                                Checkpoint.station
                                    (Checkpoint.idFromInt (draft.nextId + index))
                                    name
                                    km
                                    NoCutoff
                            )
            in
            { draft
                | checkpoints =
                    pinEnds
                        (List.filter (\checkpoint -> checkpoint.role /= Station) draft.checkpoints
                            ++ Checkpoint.sortByKm seeded
                        )
                , nextId = draft.nextId + List.length seeded
            }



-- VIEW


view : SetupState -> Html Msg
view state =
    let
        hasCourse =
            state.draft.route /= Nothing
    in
    div []
        [ step True
            "Bước 1"
            "Nạp đường chạy"
            "File GPX của giải. App đọc luôn cự ly, độ cao và các trạm nếu file có sẵn."
            (courseSection state)
        , step hasCourse
            "Bước 2"
            "Ngày giờ xuất phát"
            "Đặt trước vài ngày cũng được. Giờ đóng trạm sau nửa đêm sẽ tự tính sang ngày hôm sau."
            (startSection state)
        , step hasCourse
            "Bước 3"
            "Các trạm và giờ đóng trạm"
            "Điền COT theo đúng bảng của BTC. Trạm nước thường không có giờ đóng — cứ để trống ô giờ. Dùng nút ↑ ↓ để xếp thứ tự trạm."
            (checkpointSection state)
        , step True
            "Bước 4"
            "Bắt đầu"
            (readySummary state.draft)
            startButtons
        ]


{-| A step that needs the course first stays visible so the runner can see
what is coming, but greyed out and inert until a GPX file or a saved plan
brings a route in. `inert` blocks clicks, taps and keyboard focus alike.
-}
step : Bool -> String -> String -> String -> List (Html Msg) -> Html Msg
step enabled number title description body =
    div
        (classList [ ( "step", True ), ( "off", not enabled ) ]
            :: (if enabled then
                    []

                else
                    [ Html.Attributes.attribute "inert" "" ]
               )
        )
        (div [ class "step-no" ] [ text number ]
            :: Theme.sectionTitle title
            :: Theme.note description
            :: body
        )


{-| What a box shows: the raw text while it has focus, the parsed value
otherwise.
-}
shown : SetupState -> Field -> String -> String
shown state field formatted =
    case state.typing of
        Just typing ->
            if typing.field == field then
                typing.text

            else
                formatted

        Nothing ->
            formatted


typedInto : Field -> String -> Msg
typedInto field raw =
    SettingChanged (Typed field raw)


done : Msg
done =
    SettingChanged CommitTyping


courseSection : SetupState -> List (Html Msg)
courseSection state =
    case state.draft.route of
        Nothing ->
            [ Form.tallButton "Chọn file GPX" (SettingChanged PickGpxFile)
            , seededDropdown
            ]

        Just course ->
            let
                ( low, high ) =
                    Route.elevationRange course
            in
            [ Form.button "Chọn file GPX khác" (SettingChanged PickGpxFile)
            , seededDropdown
            , div [ class "facts" ]
                [ fact (Km.toString (Route.totalKm course)) "Km"
                , fact (String.fromInt (Elevation.inWholeMeters (Route.totalAscent course))) "M leo"
                , fact (String.fromInt (Elevation.inWholeMeters (Route.totalDescent course))) "M xuống"
                ]
            , div [ class "profile-card" ]
                [ Profile.view
                    (Pointer.scrub (\x w -> SettingChanged (ScrubSetup x w)))
                    { route = course
                    , checkpoints = state.draft.checkpoints
                    , you = Nothing
                    , cursor = cursorKm state
                    }
                , div [ class "profile-legend" ]
                    [ Html.span [] [ text (String.fromInt (round low) ++ "m") ]
                    , Html.span [] [ text "Mặt cắt độ cao" ]
                    , Html.span [] [ text (String.fromInt (round high) ++ "m") ]
                    ]
                , div [ class "scrub-read", classList [ ( "live", state.scrub /= NotScrubbing ) ] ]
                    [ text
                        (case state.scrub of
                            NotScrubbing ->
                                "Chạm và rê ngón tay trên biểu đồ để xem độ cao từng km"

                            ScrubbingAt km ->
                                Profile.readout course km
                        )
                    ]
                ]
            , if List.isEmpty (Route.waypoints course) then
                text ""

              else
                Form.miniButton
                    ("Nạp "
                        ++ String.fromInt (List.length (Route.waypoints course))
                        ++ " trạm có sẵn trong file"
                    )
                    (SettingChanged SeedFromWaypoints)
            ]


{-| The bundled race plans, offered right where a file would be chosen.
-}
seededDropdown : Html Msg
seededDropdown =
    Form.dropdown "Harvest Moon"
        (List.map .label Seeded.plans)
        (SettingChanged << SeedPlanChosen)


cursorKm : SetupState -> Maybe Km.Km
cursorKm state =
    case state.scrub of
        NotScrubbing ->
            Nothing

        ScrubbingAt km ->
            Just km


fact : String -> String -> Html Msg
fact value label =
    div [ class "fact" ]
        [ div [ class "v" ] [ text value ]
        , div [ class "k" ] [ text label ]
        ]


startSection : SetupState -> List (Html Msg)
startSection state =
    [ Form.field "Ngày"
        (Form.pair
            [ Form.clockField "18/08/2026"
                (shown state DateField (Maybe.map DateOnly.toString state.draft.date |> Maybe.withDefault ""))
                (typedInto DateField)
                done
            , Form.button "Hôm nay" (SettingChanged UseToday)
            , Form.button "Mai" (SettingChanged UseTomorrow)
            ]
        )
    , Form.field "Giờ rời vạch xuất phát"
        (Form.pair
            [ Form.clockField "05:30"
                (shown state TimeField (Maybe.map Clock.toString state.draft.time |> Maybe.withDefault ""))
                (typedInto TimeField)
                done
            , Form.button "Bây giờ" (SettingChanged UseCurrentTime)
            ]
        )
    , Theme.note "Gõ 4 số cho giờ cũng được, ví dụ 0530."
    ]


{-| Keyed by checkpoint id: when a card moves, its DOM node moves with it, so
a focused input keeps its focus and its half-typed text through the move.
-}
checkpointSection : SetupState -> List (Html Msg)
checkpointSection state =
    let
        stationIds =
            state.draft.checkpoints
                |> List.filter (\checkpoint -> checkpoint.role == Station)
                |> List.map .id

        edges checkpoint =
            { atTop = List.head stationIds == Just checkpoint.id
            , atBottom = List.head (List.reverse stationIds) == Just checkpoint.id
            }
    in
    [ Html.Keyed.node "div"
        [ class "cp-edit" ]
        (List.map
            (\checkpoint ->
                ( Checkpoint.idToString checkpoint.id
                , checkpointCard state (edges checkpoint) checkpoint
                )
            )
            state.draft.checkpoints
        )
    , Form.miniButton "Thêm một trạm" (SettingChanged AddStation)
    , Form.field "Quy đổi dốc: 100 m leo bằng bao nhiêu mét đường bằng?"
        (Form.typedNumberField "1000"
            (shown state ClimbField (String.fromInt state.draft.climbRatio))
            (typedInto ClimbField)
            done
        )
    , Theme.note "App dùng số này để tính màu cho trạng thái COT khi chạy."
    ]


checkpointCard : SetupState -> { atTop : Bool, atBottom : Bool } -> Checkpoint -> Html Msg
checkpointCard state edges checkpoint =
    div [ class "cp-card", classList [ ( "pinned", checkpoint.role /= Station ) ] ]
        [ div [ class "r1" ]
            (Form.textField
                (if checkpoint.role == Station then
                    "Tên trạm"

                 else
                    ""
                )
                checkpoint.name
                (SettingChanged << EditName checkpoint.id)
                :: controls edges checkpoint
            )
        , div [ class "r2" ]
            (div []
                [ Html.label [ class "lbl" ] [ text "Km" ]
                , if checkpoint.role == Station then
                    Form.typedNumberField "km"
                        (shown state (KmOf checkpoint.id) (kmText checkpoint))
                        (typedInto (KmOf checkpoint.id))
                        done

                  else
                    div [ class "km-lock" ] [ text ("km " ++ Km.toString checkpoint.km) ]
                ]
                :: div []
                    [ Html.label [ class "lbl" ]
                        [ text (cutoffLabel checkpoint) ]
                    , if checkpoint.role == StartLine then
                        div [ class "km-lock" ] [ text "Theo bước 2" ]

                      else
                        Form.clockField "05:30"
                            (shown state
                                (CutoffOf checkpoint.id)
                                (Maybe.map Clock.toString (Checkpoint.cutoffClock checkpoint) |> Maybe.withDefault "")
                            )
                            (typedInto (CutoffOf checkpoint.id))
                            done
                    ]
                :: targetColumn state checkpoint
            )
        ]


{-| The runner's own target time. The start has no target: it is when the gun
goes.
-}
targetColumn : SetupState -> Checkpoint -> List (Html Msg)
targetColumn state checkpoint =
    if checkpoint.role == StartLine then
        []

    else
        [ div []
            [ Html.label [ class "lbl" ] [ text "Mục tiêu" ]
            , Form.clockField "05:30"
                (shown state
                    (TargetOf checkpoint.id)
                    (Maybe.map Clock.toString checkpoint.target |> Maybe.withDefault "")
                )
                (typedInto (TargetOf checkpoint.id))
                done
            ]
        ]


{-| A station that has no km yet shows the placeholder, not "0.0".
-}
kmText : Checkpoint -> String
kmText checkpoint =
    if Km.toFloat checkpoint.km > 0 then
        Km.toString checkpoint.km

    else
        ""


cutoffLabel : Checkpoint -> String
cutoffLabel checkpoint =
    case checkpoint.role of
        StartLine ->
            "Giờ xuất phát"

        FinishLine ->
            "COT (để trống nếu không có)"

        Station ->
            "COT (để trống nếu không có)"


controls : { atTop : Bool, atBottom : Bool } -> Checkpoint -> List (Html Msg)
controls edges checkpoint =
    case checkpoint.role of
        StartLine ->
            [ Html.span [ class "cp-tag" ] [ text "XP" ] ]

        FinishLine ->
            [ Html.span [ class "cp-tag" ] [ text "ĐÍCH" ] ]

        Station ->
            [ Form.iconButton "↑" edges.atTop (SettingChanged (MoveStationUp checkpoint.id))
            , Form.iconButton "↓" edges.atBottom (SettingChanged (MoveStationDown checkpoint.id))
            , Form.iconButton "✕" False (SettingChanged (RemoveStation checkpoint.id))
            ]


readySummary : Draft -> String
readySummary draft =
    case Plan.fromDraft draft of
        Err problems ->
            String.join " " (List.map Plan.issueText (NonEmpty.toList problems))

        Ok plan ->
            let
                withCutoff =
                    NonEmpty.toList (Plan.checkpoints plan)
                        |> List.filter
                            (\checkpoint ->
                                Checkpoint.hasCutoff checkpoint && checkpoint.role /= StartLine
                            )
            in
            String.fromInt (NonEmpty.length (Plan.checkpoints plan))
                ++ " trạm, "
                ++ String.fromInt (List.length withCutoff)
                ++ " trạm có giờ đóng trạm. Xuất phát "
                ++ DateOnly.toString (Plan.date plan)
                ++ " lúc "
                ++ Clock.toString (Plan.time plan)
                ++ "."


startButtons : List (Html Msg)
startButtons =
    [ Form.button "Soát lại kế hoạch" (SettingChanged ReviewPlan)
    , Form.tallButton "Vào chế độ chạy" (SettingChanged StartRace)
    , Form.pair
        [ Form.miniButton "Lưu kế hoạch" (SettingChanged ExportPlan)
        , Form.miniButton "Mở kế hoạch" (SettingChanged PickPlanFile)
        ]
    ]
