module Main exposing (main)

{-| Wiring only. Every decision lives in `Core`, every screen in `Page`, every
contact with the outside world in `Runtime.Ports`.
-}

import Action exposing (Msg(..))
import Browser
import Core.App.Km as Km
import Core.App.Plan as Plan
import Core.App.Position as Position
import Core.App.Progress as Progress
import Core.App.Route as Route
import Core.Data.Distance as Distance
import Core.Data.Duration as Duration
import Core.Data.NonEmpty as NonEmpty
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Page.About as About
import Page.Racing as Racing
import Page.Setting as Setting
import Runtime.Gps as Gps
import Runtime.Gpx as Gpx
import Runtime.Ports as Ports
import State exposing (Dialog(..), Model, Screen(..))
import Storage.Snapshot as Snapshot
import Task
import Time
import View.Form as Form
import View.Theme as Theme


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| Flags carry whatever was in local storage. A snapshot that fails to decode
is discarded rather than half applied.
-}
init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        base =
            State.initialModel Time.utc (Time.millisToPosix 0)

        restored =
            Decode.decodeValue (Decode.field "snapshot" Snapshot.decoder) flags
    in
    ( case restored of
        Ok snapshot ->
            Snapshot.restore snapshot base

        Err _ ->
            base
    , Cmd.batch
        [ Task.perform GotZone Time.here
        , Task.perform Tick Time.now
        ]
    )


{-| Every message goes through `step`; this wrapper only decides whether the
result is worth writing down. It is, exactly when the part of the model that
`Storage.Snapshot` keeps has changed. A clock tick or a finger on the chart
changes nothing there, so neither touches storage.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( next, cmd ) =
            step msg model

        after =
            Snapshot.capture next
    in
    if after == Snapshot.capture model then
        ( next, cmd )

    else
        ( next, Cmd.batch [ cmd, Ports.save (Snapshot.encode after) ] )


step : Msg -> Model -> ( Model, Cmd Msg )
step msg model =
    case msg of
        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        Tick now ->
            ( { model | now = now }, Cmd.none )

        SettingChanged inner ->
            case model.screen of
                Setting state ->
                    Setting.update inner state model

                Racing _ ->
                    ( model, Cmd.none )

        RacingChanged inner ->
            case model.screen of
                Racing race ->
                    Racing.update inner race model

                Setting _ ->
                    ( model, Cmd.none )

        GpsArrived raw ->
            let
                settled =
                    case model.screen of
                        Racing race ->
                            { model | screen = Racing { race | gpsPending = False } }

                        Setting _ ->
                            model
            in
            case ( Decode.decodeValue Gps.decoder raw, settled.screen ) of
                ( Ok (Ok fix), Racing race ) ->
                    Racing.applyGps fix race settled

                ( Ok (Err error), _ ) ->
                    ( notify (Gps.errorMessage error) settled, Cmd.none )

                _ ->
                    ( notify "Không lấy được vị trí." settled, Cmd.none )

        GpxArrived raw ->
            case ( Decode.decodeValue Gpx.decoder raw, model.screen ) of
                ( Ok (Ok payload), Setting state ) ->
                    case Route.fromSamples payload.samples payload.waypoints of
                        Ok course ->
                            ( { model
                                | screen =
                                    Setting { state | draft = Plan.draftWithRoute course state.draft }
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( notify (Route.errorMessage error) model, Cmd.none )

                ( Ok (Err error), _ ) ->
                    ( notify (Gpx.errorMessage error) model, Cmd.none )

                _ ->
                    ( notify "Không đọc được file GPX." model, Cmd.none )

        ShowAbout ->
            ( { model | dialog = Just About }, Cmd.none )

        CloseDialog ->
            ( { model | dialog = Nothing }, Cmd.none )


notify : String -> Model -> Model
notify content model =
    { model | toast = Just { text = content, shownAt = model.now } }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.gpsIn GpsArrived
        , Ports.gpxIn GpxArrived
        , Time.every 20000 Tick
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "backdrop" ] []
        , div [ class "wrap" ]
            [ header model
            , case model.screen of
                Setting state ->
                    Setting.view state

                Racing race ->
                    Racing.view model race
            , footer
            ]
        , dialogLayer model
        , toastLayer model
        ]


header : Model -> Html Msg
header model =
    Html.header [ class "bar" ]
        [ div [ class "bar-brand" ]
            [ div [ class "mark" ] [ text "TRẠM", Html.b [] [ text "KẾ" ] ]
            , Html.a
                [ class "bar-link"
                , Html.Attributes.href About.repositoryUrl
                , Html.Attributes.target "_blank"
                , Html.Attributes.rel "noopener noreferrer"
                ]
                [ text "GitHub" ]
            ]
        , div [ class "freshness" ] [ text (freshness model) ]
        ]


freshness : Model -> String
freshness model =
    case model.screen of
        Setting _ ->
            ""

        Racing race ->
            let
                minutes =
                    floor (Duration.inMinutes (Progress.age model.now race.progress))
            in
            "km "
                ++ Km.toString (Progress.km race.progress)
                ++ " · cập nhật "
                ++ (if minutes < 1 then
                        "vừa xong"

                    else
                        String.fromInt minutes ++ " phút trước"
                   )


footer : Html Msg
footer =
    Html.footer [ class "fine" ]
        [ text "Chạy hoàn toàn offline · Không gửi dữ liệu đi đâu"
        , Html.br [] []
        , Html.button [ class "link", Html.Events.onClick ShowAbout ] [ text "Về ứng dụng · Góp ý trên GitHub" ]
        ]


toastLayer : Model -> Html Msg
toastLayer model =
    case model.toast of
        Nothing ->
            text ""

        Just toast ->
            div [ class "toast" ] [ text toast.text ]


dialogLayer : Model -> Html Msg
dialogLayer model =
    case model.dialog of
        Nothing ->
            text ""

        Just (PlanReview issues canStart) ->
            sheet
                (if List.isEmpty issues then
                    "Kế hoạch không có gì bất thường"

                 else
                    "Có chỗ cần xem lại"
                )
                "Máy chỉ soát được logic của các con số, không biết bảng COT thật của BTC ghi gì."
                (List.map issueRow issues
                    ++ [ Form.pair
                            (Form.button "Quay lại sửa" CloseDialog
                                :: (if canStart then
                                        -- Blocking issues make starting impossible, so the
                                        -- override only overrides advisories.
                                        [ Form.solidButton "Vẫn bắt đầu"
                                            (List.any Plan.isBlocking issues)
                                            (SettingChanged Action.ConfirmStartRace)
                                        ]

                                    else
                                        []
                                   )
                            )
                       ]
                )

        Just (PickPosition options) ->
            sheet "Bạn đang ở đoạn nào?"
                "Chỗ này đường chạy đi qua nhiều lần nên có nhiều khả năng."
                (List.map candidateRow (NonEmpty.toList options)
                    ++ [ Form.button "Giữ nguyên km hiện tại" (RacingChanged Action.KeepPosition) ]
                )

        Just ConfirmQuit ->
            sheet "Kết thúc"
                "Bạn muốn làm gì với kế hoạch hiện tại?"
                [ Form.button "Chạy lại từ đầu" (RacingChanged Action.QuitToSetup)
                , Form.button "Xoá hết" (RacingChanged Action.QuitAndErase)
                , Form.quietButton "Quay lại" CloseDialog
                ]

        Just About ->
            div [ class "veil" ]
                [ div [ class "sheet" ] [ About.view CloseDialog ] ]


sheet : String -> String -> List (Html Msg) -> Html Msg
sheet title description body =
    div [ class "veil mid" ]
        [ div [ class "sheet center" ]
            (Html.h3 [] [ text title ] :: Theme.note description :: body)
        ]


issueRow : Plan.Issue -> Html Msg
issueRow issue =
    div
        [ class
            ("issue "
                ++ (if Plan.isBlocking issue then
                        "bad"

                    else
                        "soft"
                   )
            )
        ]
        [ Html.span []
            [ text
                (if Plan.isBlocking issue then
                    "✕"

                 else
                    "!"
                )
            ]
        , div [] [ text (Plan.issueText issue) ]
        ]


candidateRow : Position.Candidate -> Html Msg
candidateRow candidate =
    Html.button
        [ class "choice", Html.Events.onClick (RacingChanged (Action.ChoosePosition candidate)) ]
        [ Html.b [] [ text ("km " ++ Km.toString (Position.candidateKm candidate)) ]
        , Html.span []
            [ text
                ("cách vệt "
                    ++ String.fromInt (Distance.inWholeMeters (Position.candidateDeviation candidate))
                    ++ " m"
                )
            ]
        ]
