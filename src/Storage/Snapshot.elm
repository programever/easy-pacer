module Storage.Snapshot exposing (Snapshot, capture, decoder, encode, restore)

{-| What survives closing or refreshing the page: the plan being built, and if
a race is under way, when it started, how far the runner has got, which
checkpoints they have passed and when, and the phone number for the help
message.

`capture` projects the model onto exactly what is worth keeping. `Main` saves
whenever that projection changes and not otherwise, so dragging a finger
across the chart never touches storage, and correcting a km always does.

Versioned so that a snapshot written by an older build fails to decode rather
than loading half of itself into a running race.

-}

import Core.App.Checkpoint as Checkpoint exposing (Checkpoint)
import Core.App.Km as Km
import Core.App.LatLon exposing (LatLon)
import Core.App.Plan as Plan exposing (Draft)
import Core.App.Progress as Progress exposing (Fix, Source(..))
import Core.Data.Distance as Distance
import Core.Data.NonEmpty as NonEmpty
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import State exposing (Model, Screen(..))
import Storage.PlanFile as PlanFile
import Time


currentVersion : Int
currentVersion =
    2


type alias Snapshot =
    { draft : Draft
    , race : Maybe Race
    , sosPhone : String
    }


type alias Race =
    { startedAt : Time.Posix
    , progress : Progress.Stored
    , passed : List ( Int, Time.Posix )
    }


capture : Model -> Snapshot
capture model =
    case model.screen of
        Setting state ->
            { draft = state.draft, race = Nothing, sosPhone = model.sosPhone }

        Racing race ->
            { draft = Plan.toDraft race.plan 1000
            , race =
                Just
                    { startedAt = race.startedAt
                    , progress = Progress.toStored race.progress
                    , passed =
                        NonEmpty.toList (Plan.checkpoints race.plan)
                            |> List.filterMap
                                (\checkpoint ->
                                    Checkpoint.passedAt checkpoint
                                        |> Maybe.map (\at -> ( Checkpoint.idToInt checkpoint.id, at ))
                                )
                    }
            , sosPhone = model.sosPhone
            }


{-| Back to where the runner was. A race whose plan no longer builds, which
cannot happen from this build's own snapshots but could from a hand edited
one, drops back to setup with the draft intact rather than failing.
-}
restore : Snapshot -> Model -> Model
restore snapshot model =
    let
        withPhone =
            { model | sosPhone = snapshot.sosPhone }

        setup =
            { withPhone | screen = Setting (State.setup snapshot.draft) }
    in
    case snapshot.race of
        Nothing ->
            setup

        Just race ->
            case Plan.fromDraft snapshot.draft of
                Err _ ->
                    setup

                Ok plan ->
                    { withPhone
                        | screen =
                            Racing
                                { plan = Plan.withCheckpoints (NonEmpty.map (markPassed race.passed) (Plan.checkpoints plan)) plan
                                , startedAt = race.startedAt
                                , progress = Progress.fromStored race.progress
                                , tab = State.PlanTab
                                , map = State.fitAll (Plan.route plan)
                                , gesture = State.noGesture
                                , scrub = State.NotScrubbing
                                , kmEntryOpen = False
                                , kmEntryText = ""
                                }
                    }


markPassed : List ( Int, Time.Posix ) -> Checkpoint -> Checkpoint
markPassed passed checkpoint =
    case List.filter (\( id, _ ) -> id == Checkpoint.idToInt checkpoint.id) passed of
        ( _, at ) :: _ ->
            { checkpoint | status = Checkpoint.Passed at }

        [] ->
            checkpoint



-- ENCODE


encode : Snapshot -> Encode.Value
encode snapshot =
    Encode.object
        [ ( "version", Encode.int currentVersion )
        , ( "plan", PlanFile.encode snapshot.draft )
        , ( "race", Maybe.map encodeRace snapshot.race |> Maybe.withDefault Encode.null )
        , ( "sosPhone", Encode.string snapshot.sosPhone )
        ]


encodeRace : Race -> Encode.Value
encodeRace race =
    Encode.object
        [ ( "startedAt", encodePosix race.startedAt )
        , ( "progress", encodeProgress race.progress )
        , ( "passed"
          , Encode.list
                (\( id, at ) -> Encode.object [ ( "id", Encode.int id ), ( "at", encodePosix at ) ])
                race.passed
          )
        ]


encodeProgress : Progress.Stored -> Encode.Value
encodeProgress stored =
    Encode.object
        [ ( "km", Encode.float (Km.toFloat stored.km) )
        , ( "updatedAt", encodePosix stored.updatedAt )
        , ( "fix"
          , case stored.source of
                FromGps fix ->
                    encodeFix fix

                FromRunner ->
                    Encode.null
          )
        , ( "breadcrumbs", Encode.list encodeFix stored.breadcrumbs )
        , ( "lastOnRoute", Maybe.map encodeLatLon stored.lastOnRoute |> Maybe.withDefault Encode.null )
        ]


encodePosix : Time.Posix -> Encode.Value
encodePosix moment =
    Encode.int (Time.posixToMillis moment)


encodeFix : Fix -> Encode.Value
encodeFix fix =
    Encode.object
        [ ( "lat", Encode.float fix.at.lat )
        , ( "lon", Encode.float fix.at.lon )
        , ( "accuracy", Encode.float (Distance.inMeters fix.accuracy) )
        , ( "taken", encodePosix fix.taken )
        ]


encodeLatLon : LatLon -> Encode.Value
encodeLatLon position =
    Encode.object
        [ ( "lat", Encode.float position.lat )
        , ( "lon", Encode.float position.lon )
        ]



-- DECODE


decoder : Decoder Snapshot
decoder =
    Decode.field "version" Decode.int
        |> Decode.andThen
            (\version ->
                if version == currentVersion then
                    body

                else
                    Decode.fail "Bản lưu thuộc phiên bản khác."
            )


body : Decoder Snapshot
body =
    Decode.map3 Snapshot
        (Decode.field "plan" PlanFile.decoder)
        (Decode.field "race" (Decode.nullable raceDecoder))
        (Decode.oneOf [ Decode.field "sosPhone" Decode.string, Decode.succeed "" ])


raceDecoder : Decoder Race
raceDecoder =
    Decode.map3 Race
        (Decode.field "startedAt" posixDecoder)
        (Decode.field "progress" progressDecoder)
        (Decode.field "passed"
            (Decode.list
                (Decode.map2 Tuple.pair
                    (Decode.field "id" Decode.int)
                    (Decode.field "at" posixDecoder)
                )
            )
        )


progressDecoder : Decoder Progress.Stored
progressDecoder =
    Decode.map5 Progress.Stored
        (Decode.field "km" Decode.float |> Decode.map Km.fromFloat)
        (Decode.field "updatedAt" posixDecoder)
        (Decode.field "fix" (Decode.nullable fixDecoder)
            |> Decode.map (Maybe.map FromGps >> Maybe.withDefault FromRunner)
        )
        (Decode.field "breadcrumbs" (Decode.list fixDecoder))
        (Decode.field "lastOnRoute" (Decode.nullable latLonDecoder))


posixDecoder : Decoder Time.Posix
posixDecoder =
    Decode.int |> Decode.map Time.millisToPosix


fixDecoder : Decoder Fix
fixDecoder =
    Decode.map3
        (\position accuracy taken -> { at = position, accuracy = accuracy, taken = taken })
        latLonDecoder
        (Decode.field "accuracy" Decode.float |> Decode.map Distance.fromMeters)
        (Decode.field "taken" posixDecoder)


latLonDecoder : Decoder LatLon
latLonDecoder =
    Decode.map2 LatLon
        (Decode.field "lat" Decode.float)
        (Decode.field "lon" Decode.float)
