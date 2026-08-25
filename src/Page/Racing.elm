module Page.Racing exposing (applyGps, update, view)

{-| The screen a runner actually looks at, mid-race, tired, one handed.

Four numbers and nothing else: how far to the next checkpoint, how much climb
and descent to get there, and how long until it closes.

-}

import Action exposing (Msg(..), RacingMsg(..))
import Core.App.Checkpoint as Checkpoint exposing (Checkpoint)
import Core.App.Km as Km exposing (Km)
import Core.App.LatLon as LatLon
import Core.App.Plan as Plan
import Core.App.Position as Position exposing (Candidate, Resolution(..), RouteState(..))
import Core.App.Progress as Progress exposing (Fix, Source(..))
import Core.App.Route as Route
import Core.App.Segment as Segment exposing (Segment, Urgency(..))
import Core.App.Sos as Sos
import Core.Data.Clock as Clock
import Core.Data.Distance as Distance
import Core.Data.Duration as Duration
import Core.Data.Elevation as Elevation
import Core.Data.NonEmpty as NonEmpty
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Runtime.Ports as Ports
import State exposing (Dialog(..), Model, RaceState, Screen(..), Scrub(..), Tab(..))
import Time
import View.Form as Form
import View.Map as Map
import View.Pointer as Pointer
import View.Profile as Profile
import View.Theme as Theme



-- UPDATE


update : RacingMsg -> RaceState -> Model -> ( Model, Cmd Msg )
update msg race model =
    case msg of
        SwitchTab wantsPlan ->
            ( put
                { race
                    | tab =
                        if wantsPlan then
                            PlanTab

                        else
                            LocateTab
                }
                model
            , Cmd.none
            )

        RequestGps ->
            ( put { race | gpsPending = True } model, Ports.requestGps () )

        ChoosePosition candidate ->
            case acceptCandidate candidate race of
                Nothing ->
                    ( { model | dialog = Nothing }, Cmd.none )

                Just ( updated, report ) ->
                    ( notifyMaybe report (put updated { model | dialog = Nothing }), Cmd.none )

        KeepPosition ->
            ( { model | dialog = Nothing }, Cmd.none )

        ToggleKmEntry ->
            ( put { race | kmEntryOpen = not race.kmEntryOpen } model, Cmd.none )

        EditKmEntry raw ->
            ( put { race | kmEntryText = raw } model, Cmd.none )

        SubmitKmEntry ->
            case String.toFloat race.kmEntryText of
                Nothing ->
                    ( notify "Nhập một số km hợp lệ." model, Cmd.none )

                Just value ->
                    ( declarePosition (Km.fromFloat value) race model, Cmd.none )

        ScrubProfile offsetX elementWidth ->
            ( put
                { race
                    | scrub =
                        ScrubbingAt
                            (Profile.kmAtFraction (Plan.route race.plan)
                                (offsetX / Basics.max 1 elementWidth)
                            )
                }
                model
            , Cmd.none
            )

        MapPointerDown id at ->
            ( put { race | gesture = State.pointerDown id at race.map race.gesture } model
            , Cmd.none
            )

        MapPointerMove id at renderedWidth ->
            let
                ( gesture, map ) =
                    State.pointerMove (Plan.route race.plan)
                        (Map.unitsPerPixel renderedWidth)
                        id
                        at
                        ( race.gesture, race.map )
            in
            ( put { race | gesture = gesture, map = map } model, Cmd.none )

        MapPointerUp id ->
            ( put { race | gesture = State.pointerUp id race.gesture } model, Cmd.none )

        MapWheel deltaY ->
            ( put
                { race
                    | map =
                        State.zoomBy
                            (if deltaY < 0 then
                                1.15

                             else
                                1 / 1.15
                            )
                            (Plan.route race.plan)
                            race.map
                }
                model
            , Cmd.none
            )

        ViewMe ->
            ( put
                { race
                    | scrub = NotScrubbing
                    , map =
                        State.fitTo
                            (Route.atKm (Plan.route race.plan) (Progress.km race.progress)).plane
                }
                model
            , Cmd.none
            )

        ViewWhole ->
            ( put { race | map = State.fitAll (Plan.route race.plan) } model, Cmd.none )

        EditPhone raw ->
            ( { model | sosPhone = raw }, Cmd.none )

        SendSos ->
            case ( Progress.lastFix race.progress, String.trim model.sosPhone ) of
                ( Nothing, _ ) ->
                    ( notify "Chưa có toạ độ — bấm Lấy GPS trước." model, Cmd.none )

                ( _, "" ) ->
                    ( notify "Nhập số điện thoại người nhận trước." model, Cmd.none )

                ( Just fix, phone ) ->
                    ( model
                    , Ports.openSms
                        { phone = phone
                        , body = Sos.message model.zone fix (Just (Progress.km race.progress))
                        }
                    )

        CopySos ->
            case Progress.lastFix race.progress of
                Nothing ->
                    ( notify "Chưa có toạ độ — bấm Lấy GPS trước." model, Cmd.none )

                Just fix ->
                    ( notify "Đã chép nội dung tin." model
                    , Ports.copyText (Sos.message model.zone fix (Just (Progress.km race.progress)))
                    )

        OpenPlanEditor ->
            ( { model | screen = Setting (State.setup (Plan.toDraft race.plan 1000)) }, Cmd.none )

        RequestQuit ->
            ( { model | dialog = Just ConfirmQuit }, Cmd.none )

        QuitToSetup ->
            ( { model
                | dialog = Nothing
                , screen = Setting (State.setup (Plan.toDraft race.plan 1000))
              }
            , Cmd.none
            )

        QuitAndErase ->
            ( { model | dialog = Nothing, screen = Setting (State.setup Plan.emptyDraft) }
            , Ports.clear ()
            )


put : RaceState -> Model -> Model
put race model =
    { model | screen = Racing race }


notify : String -> Model -> Model
notify content model =
    { model | toast = Just { text = content, shownAt = model.now } }


notifyMaybe : Maybe String -> Model -> Model
notifyMaybe content model =
    Maybe.map (\text -> notify text model) content |> Maybe.withDefault model


{-| A distance the runner types is a decision, and it overrides the displayed
position too. Leaving the old dot on the map would make the next fix compare
itself against a place the runner is no longer standing.
-}
declarePosition : Km -> RaceState -> Model -> Model
declarePosition reached race model =
    let
        moved =
            Progress.fromRunner reached model.now race.progress

        undone =
            NonEmpty.filterToList
                (\checkpoint ->
                    Checkpoint.isPassed checkpoint && not (Km.isAtOrBefore reached checkpoint.km)
                )
                (Plan.checkpoints race.plan)
    in
    put
        { race
            | progress = moved
            , plan = syncPlan reached model.now race.plan
            , kmEntryOpen = False
            , map = State.fitTo (Route.atKm (Plan.route race.plan) reached).plane
        }
        (notify
            ("Đã đặt lại về km "
                ++ Km.toString reached
                ++ (if List.isEmpty undone then
                        ""

                    else
                        " · "
                            ++ String.fromInt (List.length undone)
                            ++ " trạm trở lại chưa qua"
                   )
            )
            model
        )


syncPlan : Km -> Time.Posix -> Plan.Plan -> Plan.Plan
syncPlan reached now plan =
    Plan.withCheckpoints
        (NonEmpty.map (Checkpoint.syncStatus reached now) (Plan.checkpoints plan))
        plan


{-| One fix answers two separate questions, so it is resolved into two separate
candidates and each is used for exactly one thing.
-}
applyGps : Fix -> RaceState -> Model -> ( Model, Cmd Msg )
applyGps fix race model =
    case Position.resolve (Plan.route race.plan) (Just race.startedAt) race.progress fix of
        Nothing ->
            ( notify "Không đối chiếu được với đường chạy." model, Cmd.none )

        Just (Ambiguous details) ->
            ( { model | dialog = Just (PickPosition details.options) }, Cmd.none )

        Just (Resolved details) ->
            let
                ( updated, report ) =
                    acceptResolution details.progress details.nearest fix race
            in
            ( notifyMaybe report (put updated model), Cmd.none )


acceptCandidate : Candidate -> RaceState -> Maybe ( RaceState, Maybe String )
acceptCandidate candidate race =
    Progress.lastFix race.progress
        |> Maybe.map (\fix -> acceptResolution candidate candidate fix race)


{-| Apply a resolved fix, and say what was done with it. The km only moves on
a fix that passes `Position.isTrustworthy`; a rejected reading is still kept
for guidance, and the runner is told why the number did not change, because a
button that does nothing in silence is a button that gets pressed ten times.
-}
acceptResolution : Candidate -> Candidate -> Fix -> RaceState -> ( RaceState, Maybe String )
acceptResolution forProgress forGuidance fix race =
    let
        state =
            Position.routeState fix forGuidance

        trustworthy =
            Position.isTrustworthy fix forGuidance

        before =
            Progress.km race.progress

        reached =
            if trustworthy then
                Position.candidateKm forProgress

            else
                before

        accuracy =
            "±" ++ String.fromInt (Distance.inWholeMeters fix.accuracy) ++ " m"

        deviation =
            String.fromInt (Distance.inWholeMeters (Position.candidateDeviation forGuidance)) ++ " m"

        gained =
            Distance.inKilometers (Km.difference before reached)

        imprecise =
            Distance.isGreaterThan Position.acceptableAccuracy fix.accuracy

        -- An imprecise fix gets no toast: the guide panel explains it in
        -- full, in place, and the tab switch below puts that panel on screen.
        report =
            if imprecise then
                Nothing

            else
                Just
                    (case state of
                        OffRoute ->
                            "Cách vệt " ++ deviation ++ " (" ++ accuracy ++ ") — xem hướng quay lại."

                        Uncertain ->
                            "Cách vệt " ++ deviation ++ " (" ++ accuracy ++ ") — chưa kết luận được, đứng yên thử lại."

                        OnRoute ->
                            "km "
                                ++ Km.toString reached
                                ++ (if gained > 0.05 && Km.isBefore reached before then
                                        " · thêm " ++ Km.toString (Km.fromFloat gained) ++ " km"

                                    else
                                        " · chưa đổi"
                                   )
                                ++ " ("
                                ++ accuracy
                                ++ ")"
                    )
    in
    ( { race
        | progress = Progress.fromGps reached fix (state == OnRoute) race.progress
        , plan = syncPlan reached fix.taken race.plan
        , tab =
            if state == OnRoute && not imprecise then
                race.tab

            else
                LocateTab
      }
    , report
    )



-- VIEW


view : Model -> RaceState -> Html Msg
view model race =
    let
        reached =
            Progress.km race.progress
    in
    div []
        [ tabs race
        , case race.tab of
            PlanTab ->
                planPanel model race reached

            LocateTab ->
                locatePanel race
        , dock race
        , footerButtons
        , sosPanel model race
        ]


tabs : RaceState -> Html Msg
tabs race =
    div [ class "tabs" ]
        [ Html.button
            [ classList [ ( "on", race.tab == PlanTab ) ]
            , onClick (RacingChanged (SwitchTab True))
            ]
            [ text "Kế hoạch" ]
        , Html.button
            [ classList [ ( "on", race.tab == LocateTab ) ]
            , onClick (RacingChanged (SwitchTab False))
            ]
            [ text "Định vị" ]
        ]


planPanel : Model -> RaceState -> Km -> Html Msg
planPanel model race reached =
    div []
        [ nextCard model race reached
        , ledger model race reached
        ]


cursorKm : RaceState -> Maybe Km
cursorKm race =
    case race.scrub of
        NotScrubbing ->
            Nothing

        ScrubbingAt km ->
            Just km


nextCard : Model -> RaceState -> Km -> Html Msg
nextCard model race reached =
    case Plan.nextAhead reached race.plan of
        Nothing ->
            Theme.card []
                [ div [ class "lead-top" ]
                    [ Theme.eyebrow "Trạm tiếp theo"
                    , div [ class "lead-name" ] [ text "Đã qua trạm cuối" ]
                    , div [ class "lead-dist" ] [ text "Chúc mừng bạn về đích" ]
                    ]
                ]

        Just checkpoint ->
            let
                segment =
                    Segment.toCheckpoint model.zone race.plan reached model.now checkpoint

                borrowed =
                    case segment.cutoff of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Segment.deadline model.zone race.plan reached model.now

                shownCutoff =
                    case ( segment.cutoff, borrowed ) of
                        ( Just moment, _ ) ->
                            Just moment

                        ( Nothing, Just ( _, moment ) ) ->
                            Just moment

                        _ ->
                            Nothing
            in
            Theme.card []
                [ div [ class "lead-top" ]
                    [ Theme.eyebrow "Trạm tiếp theo"
                    , div [ class "lead-name" ] [ text (Checkpoint.displayName checkpoint) ]
                    , div [ class "lead-dist" ]
                        [ text
                            ("trạm ở km "
                                ++ Km.toString checkpoint.km
                                ++ " · bạn đang ở km "
                                ++ Km.toString reached
                            )
                        ]
                    ]
                , div [ class "quad" ]
                    [ Theme.quadCell "Còn lại" (Km.toString (Km.fromFloat (Distance.inKilometers segment.distance))) "km"
                    , Theme.quadCell "Leo lên" (String.fromInt (Elevation.inWholeMeters segment.ascent)) "m"
                    , Theme.quadCell "Xuống dốc" (String.fromInt (Elevation.inWholeMeters segment.descent)) "m"
                    , Theme.quadCell (cutoffLabel borrowed)
                        (case shownCutoff of
                            Nothing ->
                                "–"

                            Just moment ->
                                Duration.toCompactString moment.remaining
                        )
                        ""
                    ]
                , targetLine checkpoint
                , cutoffVerdict model race reached borrowed segment shownCutoff
                ]


{-| The runner's own plan for this checkpoint, restated where the countdown
lives. A fact off the race sheet, not a judgement.
-}
targetLine : Checkpoint -> Html Msg
targetLine checkpoint =
    case checkpoint.target of
        Nothing ->
            text ""

        Just clock ->
            div [ class "lead-dist" ]
                [ text ("Mục tiêu của bạn: tới trạm lúc " ++ Clock.toString clock) ]


cutoffLabel : Maybe ( Checkpoint, Segment.CutoffMoment ) -> String
cutoffLabel borrowed =
    case borrowed of
        Nothing ->
            "Tới giờ đóng trạm"

        Just ( checkpoint, _ ) ->
            "Tới COT · " ++ Checkpoint.displayName checkpoint


{-| The colour and the sentence under the countdown. The judgement is made on
the segment to the checkpoint that OWNS the binding cutoff — when the next
stop is a water station, being green to the station means nothing if the real
deadline further on demands a sprint.
-}
cutoffVerdict : Model -> RaceState -> Km -> Maybe ( Checkpoint, Segment.CutoffMoment ) -> Segment -> Maybe Segment.CutoffMoment -> Html Msg
cutoffVerdict model race reached borrowed segment shownCutoff =
    let
        ratio =
            Plan.climbRatio race.plan

        binding =
            case borrowed of
                Just ( checkpoint, _ ) ->
                    Segment.toCheckpoint model.zone race.plan reached model.now checkpoint

                Nothing ->
                    segment

        pace =
            Segment.requiredPace ratio binding
                |> Maybe.map (\value -> "cần nhanh hơn " ++ paceText value ++ " phút/km (đã quy đổi dốc)")
    in
    case ( borrowed, shownCutoff ) of
        ( Just ( checkpoint, moment ), _ ) ->
            Theme.verdict (Segment.urgency ratio binding)
                "Trạm này không có giờ đóng trạm"
                (Just
                    ("Mốc gần nhất là "
                        ++ Checkpoint.displayName checkpoint
                        ++ " ở km "
                        ++ Km.toString checkpoint.km
                        ++ ", đóng lúc "
                        ++ clockOf model moment.closesAt
                        ++ (Maybe.map (\line -> " — " ++ line) pace |> Maybe.withDefault "")
                    )
                )

        ( Nothing, Just moment ) ->
            if Duration.isNegative moment.remaining then
                Theme.verdict Missed ("Đã quá giờ đóng trạm " ++ clockOf model moment.closesAt) Nothing

            else
                Theme.verdict (Segment.urgency ratio binding)
                    ("Đóng trạm lúc " ++ clockOf model moment.closesAt)
                    pace

        _ ->
            Theme.verdict NoDeadline "Phía trước không còn trạm nào có giờ đóng trạm" Nothing


{-| One decimal, with the Vietnamese decimal comma.
-}
paceText : Float -> String
paceText pace =
    String.fromFloat (Basics.toFloat (round (pace * 10)) / 10)
        |> String.replace "." ","


clockOf : Model -> Time.Posix -> String
clockOf model moment =
    pad (Time.toHour model.zone moment) ++ ":" ++ pad (Time.toMinute model.zone moment)


pad : Int -> String
pad value =
    String.padLeft 2 '0' (String.fromInt value)


scrubReadout : RaceState -> String
scrubReadout race =
    case race.scrub of
        NotScrubbing ->
            "Chạm và rê ngón tay trên biểu đồ để xem dốc phía trước"

        ScrubbingAt km ->
            Profile.readout (Plan.route race.plan) km


ledger : Model -> RaceState -> Km -> Html Msg
ledger model race reached =
    div [ class "ledger" ]
        [ Html.h3 [] [ text "Toàn bộ chặng" ]
        , div []
            (NonEmpty.toList (Plan.checkpoints race.plan)
                |> Checkpoint.sortByKm
                |> List.map (ledgerRow model race reached)
            )
        ]


ledgerRow : Model -> RaceState -> Km -> Checkpoint -> Html Msg
ledgerRow model race reached checkpoint =
    let
        segment =
            Segment.toCheckpoint model.zone race.plan reached model.now checkpoint

        passed =
            Checkpoint.isPassed checkpoint

        detail =
            if passed then
                "km "
                    ++ Km.toString checkpoint.km
                    ++ (case Checkpoint.passedAt checkpoint of
                            Nothing ->
                                ""

                            Just moment ->
                                " · qua lúc " ++ clockOf model moment ++ targetMargin model race checkpoint moment
                       )

            else
                "km "
                    ++ Km.toString checkpoint.km
                    ++ " · còn "
                    ++ Km.toString (Km.fromFloat (Distance.inKilometers segment.distance))
                    ++ " km · ↑"
                    ++ String.fromInt (Elevation.inWholeMeters segment.ascent)
                    ++ " ↓"
                    ++ String.fromInt (Elevation.inWholeMeters segment.descent)
                    ++ " m"
                    ++ (case checkpoint.target of
                            Nothing ->
                                ""

                            Just clock ->
                                " · mục tiêu " ++ Clock.toString clock
                       )
    in
    Theme.ledgerRow
        [ classList [ ( "passed", passed ) ] ]
        [ div [ class "body" ]
            [ div [ class "nm" ] [ text (Checkpoint.displayName checkpoint) ]
            , div [ class "sub" ] [ text detail ]
            ]
        , amountCell model (Plan.climbRatio race.plan) checkpoint segment
        ]


{-| Passed with a target: the margin the runner actually had against their own
plan, a fixed fact like the cutoff margin.
-}
targetMargin : Model -> RaceState -> Checkpoint -> Time.Posix -> String
targetMargin model race checkpoint arrived =
    case Plan.targetMoment model.zone race.plan checkpoint of
        Nothing ->
            ""

        Just target ->
            " · mục tiêu "
                ++ clockOf model target
                ++ " ("
                ++ Duration.toSignedString (Duration.between arrived target)
                ++ ")"


{-| The right hand column. Ahead of the runner it is a countdown to the
cutoff; behind, it is the margin they actually had when they arrived, which is
a fixed fact and must not keep drifting with the clock. Both name the cutoff
time itself, because "2h10 to go" means nothing without "until 12:30".
-}
amountCell : Model -> Int -> Checkpoint -> Segment -> Html Msg
amountCell model ratio checkpoint segment =
    case ( segment.cutoff, Checkpoint.passedAt checkpoint ) of
        ( Nothing, _ ) ->
            div [ class "amt flat" ]
                [ text "—", Html.small [] [ text "không có COT" ] ]

        ( Just moment, Nothing ) ->
            div [ class ("amt " ++ Theme.urgencyClass (Segment.urgency ratio segment)) ]
                [ text (Duration.toCompactString moment.remaining)
                , Html.small [] [ text ("COT " ++ clockOf model moment.closesAt) ]
                ]

        ( Just moment, Just arrived ) ->
            let
                margin =
                    Duration.between arrived moment.closesAt
            in
            div
                [ class
                    ("amt "
                        ++ (if Duration.isNegative margin then
                                "late"

                            else
                                "ok"
                           )
                    )
                ]
                [ text (Duration.toSignedString margin)
                , Html.small [] [ text ("so với COT " ++ clockOf model moment.closesAt) ]
                ]


locatePanel : RaceState -> Html Msg
locatePanel race =
    div []
        [ guide race
        , div [ class "map-card" ]
            [ div [ class "map-view" ]
                [ Map.view
                    (Pointer.wheel (RacingChanged << MapWheel)
                        :: Pointer.drag
                            { down = \id at -> RacingChanged (MapPointerDown id at)
                            , move = \id at w -> RacingChanged (MapPointerMove id at w)
                            , up = RacingChanged << MapPointerUp
                            }
                    )
                    { route = Plan.route race.plan
                    , view = race.map
                    , checkpoints = NonEmpty.toList (Plan.checkpoints race.plan)
                    , reached = Progress.km race.progress
                    , fix = Progress.lastFix race.progress
                    , lost = currentState race == OffRoute
                    , uncertain = currentState race == Uncertain
                    , snapTo = snapTarget race
                    , cursor = cursorKm race
                    , breadcrumbs = List.map .at (Progress.breadcrumbs race.progress)
                    }
                ]
            , div [ class "map-foot" ]
                [ div [ class "scale" ] [ text Map.gestureHint ]
                , Form.miniButton "Về vị trí tôi" (RacingChanged ViewMe)
                , Form.miniButton "Toàn tuyến" (RacingChanged ViewWhole)
                ]
            ]
        , elevationCard race
        ]


{-| The course side on, under the map. Dragging across it moves a cursor on
both, so the runner can see where a climb ahead actually is on the ground.
-}
elevationCard : RaceState -> Html Msg
elevationCard race =
    let
        reached =
            Progress.km race.progress
    in
    div [ class "profile-card" ]
        [ div [ class "profile-plot" ]
            [ Profile.view
                (Pointer.scrub (\x w -> RacingChanged (ScrubProfile x w)))
                { route = Plan.route race.plan
                , checkpoints = NonEmpty.toList (Plan.checkpoints race.plan)
                , you = Just reached
                , cursor = cursorKm race
                }
            ]
        , div [ class "profile-legend" ]
            [ Html.span [] [ text "Xuất phát" ]
            , Html.span [] [ text ("Bạn ở km " ++ Km.toString reached) ]
            , Html.span [] [ text ("Về đích km " ++ Km.toString (Route.totalKm (Plan.route race.plan))) ]
            ]
        , div [ class "scrub-read", classList [ ( "live", race.scrub /= NotScrubbing ) ] ]
            [ text (scrubReadout race) ]
        ]


{-| The nearest point of the course to the last GPS fix: where the dashed
line home on the map points. The map only draws it when the runner is lost.
-}
snapTarget : RaceState -> Maybe LatLon.LatLon
snapTarget race =
    case Progress.source race.progress of
        FromRunner ->
            Nothing

        FromGps fix ->
            Position.candidates (Plan.route race.plan) fix.at
                |> List.head
                |> Maybe.map Position.candidateSnap


currentState : RaceState -> RouteState
currentState race =
    case Progress.source race.progress of
        FromRunner ->
            OnRoute

        FromGps fix ->
            case Position.candidates (Plan.route race.plan) fix.at of
                nearest :: _ ->
                    Position.routeState fix nearest

                [] ->
                    OnRoute


guide : RaceState -> Html Msg
guide race =
    case Progress.source race.progress of
        FromRunner ->
            div [ class "guide on-route" ]
                [ div [ class "head" ] [ text "VỊ TRÍ DO BẠN TỰ NHẬP" ]
                , div [ class "detail" ]
                    [ text
                        ("Đang lấy mốc km "
                            ++ Km.toString (Progress.km race.progress)
                            ++ " trên đường chạy. Bấm Lấy GPS nếu muốn máy tự xác định lại."
                        )
                    ]
                ]

        FromGps fix ->
            case Position.candidates (Plan.route race.plan) fix.at of
                [] ->
                    div [ class "guide" ] [ div [ class "head" ] [ text "Chưa biết bạn ở đâu" ] ]

                nearest :: _ ->
                    guideFor race fix nearest


guideFor : RaceState -> Fix -> Candidate -> Html Msg
guideFor race fix nearest =
    let
        state =
            Position.routeState fix nearest

        deviation =
            String.fromInt (Distance.inWholeMeters (Position.candidateDeviation nearest))

        accuracyMetres =
            String.fromInt (Distance.inWholeMeters fix.accuracy)

        accuracy =
            " · sai số máy đo ±" ++ accuracyMetres ++ " m"

        -- The fix was too imprecise to move the km. Said here, in red, under
        -- whatever the panel already says, because this is where the runner
        -- is looking when they want to know why the number did not change.
        rejected =
            if Distance.isGreaterThan Position.acceptableAccuracy fix.accuracy then
                [ div [ class "reject" ]
                    [ text
                        ("Sai số ±"
                            ++ accuracyMetres
                            ++ " m quá lớn nên chưa cập nhật km. Ra chỗ thoáng rồi bấm Lấy GPS lại, hoặc nhập số km."
                        )
                    ]
                ]

            else
                []

        body =
            case state of
                OnRoute ->
                    [ div [ class "head" ] [ text "ĐANG TRÊN ĐƯỜNG CHẠY" ]
                    , div [ class "detail" ]
                        [ text
                            ("Cách vệt "
                                ++ deviation
                                ++ " m, ở khoảng km "
                                ++ Km.toString (Progress.km race.progress)
                                ++ accuracy
                                ++ ". Cứ đi tiếp."
                            )
                        ]
                    ]

                Uncertain ->
                    [ div [ class "head" ] [ text "CHƯA KẾT LUẬN ĐƯỢC" ]
                    , div [ class "detail" ]
                        [ text
                            ("Máy báo cách vệt "
                                ++ deviation
                                ++ " m"
                                ++ accuracy
                                ++ " — độ lệch còn nằm trong sai số, nên có thể bạn vẫn đang trên đường chạy."
                            )
                        ]
                    , Html.ol []
                        [ Html.li [] [ text "Đứng yên tại chỗ, tránh chỗ có tán cây dày hoặc vách đá che." ]
                        , Html.li [] [ text "Bấm Lấy GPS lại sau vài chục giây để máy bắt tín hiệu tốt hơn." ]
                        , Html.li [] [ text "Trong lúc chờ, nhìn quanh tìm dấu ruy băng của BTC." ]
                        ]
                    ]

                OffRoute ->
                    let
                        heading =
                            LatLon.bearing fix.at (Position.candidateSnap nearest)

                        direction =
                            LatLon.compass heading

                        backKm =
                            Position.candidateKm nearest

                        behind =
                            Km.isBefore (Progress.km race.progress) backKm
                    in
                    [ div [ class "head" ] [ text ("BẠN ĐÃ RỜI ĐƯỜNG CHẠY " ++ deviation ++ " M") ]
                    , Html.span [ class "arrow" ] [ text (LatLon.compassArrow direction) ]
                    , div [ class "detail" ]
                        [ text
                            ("Đi hướng "
                                ++ LatLon.compassLabel direction
                                ++ " ("
                                ++ String.fromInt (LatLon.bearingDegrees heading)
                                ++ "°) khoảng "
                                ++ deviation
                                ++ " m để về vệt tại km "
                                ++ Km.toString backKm
                                ++ "."
                                ++ (if behind then
                                        " Điểm này nằm ở đoạn bạn đã đi qua — về tới nơi thì đi xuôi trở lại."

                                    else
                                        ""
                                   )
                            )
                        ]
                    , Html.ol []
                        [ Html.li [] [ text "Dừng lại, đừng đi thêm cho tới khi biết hướng." ]
                        , Html.li [] [ text ("Xoay người cho tới khi hướng đi khớp với " ++ LatLon.compassLabel direction ++ "; xem sơ đồ bên dưới để đối chiếu.") ]
                        , Html.li []
                            [ text
                                (if Distance.inMeters (Position.candidateDeviation nearest) > 400 then
                                    "Lệch hơn 400 m thì an toàn nhất là lần ngược theo đường bạn vừa đi (các chấm xám trên sơ đồ) về chỗ còn thấy dấu của BTC."

                                 else
                                    "Vừa đi vừa để ý dấu ruy băng của BTC hai bên đường."
                                )
                            ]
                        , Html.li [] [ text "Tới nơi bấm Lấy GPS lại để xác nhận đã về đúng vệt." ]
                        ]
                    ]
    in
    div [ class (Theme.routeStateClass state) ] (body ++ rejected)


dock : RaceState -> Html Msg
dock race =
    div [ class "dock" ]
        [ div [ class "hint" ] [ text "App chỉ đọc GPS khi bạn bấm — không chạy nền, không hao pin." ]
        , Form.pair
            [ Form.pendingButton "Lấy GPS" "Đang bắt GPS…" race.gpsPending (RacingChanged RequestGps)
            , Html.button
                [ class "btn tall", onClick (RacingChanged ToggleKmEntry) ]
                [ text "Nhập số km" ]
            ]
        , if race.kmEntryOpen then
            div [ class "km-entry" ]
                [ Form.numberField "Đã chạy bao nhiêu km?" race.kmEntryText (RacingChanged << EditKmEntry)
                , Form.solidButton "Cập nhật" False (RacingChanged SubmitKmEntry)
                ]

          else
            text ""
        ]


footerButtons : Html Msg
footerButtons =
    div [ class "step" ]
        [ Form.pair
            [ Form.miniButton "Sửa kế hoạch" (RacingChanged OpenPlanEditor)
            , Form.quietButton "Kết thúc" (RacingChanged RequestQuit)
            ]
        ]


sosPanel : Model -> RaceState -> Html Msg
sosPanel model race =
    div [ class "sos" ]
        [ div [ class "sos-head" ] [ text "Khi cần trợ giúp" ]
        , Theme.note "Soạn sẵn tin báo vị trí. Nhắn tin đi được cả khi mất 4G, miễn còn sóng điện thoại."
        , Form.pair
            [ Form.textField "Số điện thoại BTC hoặc người nhà" model.sosPhone (RacingChanged << EditPhone)
            , Form.miniButton "Nhắn tin" (RacingChanged SendSos)
            ]
        , div [ class "sos-preview" ]
            [ text
                (case Progress.lastFix race.progress of
                    Nothing ->
                        "Bấm Lấy GPS trước để có toạ độ đưa vào tin nhắn."

                    Just fix ->
                        Sos.message model.zone fix (Just (Progress.km race.progress))
                )
            ]
        , Form.miniButton "Chép nội dung tin" (RacingChanged CopySos)
        ]
