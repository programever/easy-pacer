module Storage.PlanFile exposing (decoder, encode, filename)

{-| The shareable plan file: course, checkpoints and start moment, as JSON.

This is a stored format, so it is versioned and every field is decoded. A file
written by an older build must either load or fail cleanly; it must never load
halfway.

-}

import Core.App.Checkpoint as Checkpoint exposing (Checkpoint)
import Core.App.Km as Km
import Core.App.Plan exposing (Draft)
import Core.App.Route as Route exposing (Route)
import Core.Data.Clock as Clock
import Core.Data.DateOnly as DateOnly
import Core.Data.Elevation as Elevation
import Core.Data.NonEmpty as NonEmpty
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


currentVersion : Int
currentVersion =
    1


filename : String
filename =
    "ke-hoach-trail.json"


encode : Draft -> Encode.Value
encode draft =
    Encode.object
        [ ( "version", Encode.int currentVersion )
        , ( "route"
          , case draft.route of
                Nothing ->
                    Encode.null

                Just course ->
                    encodeRoute course
          )
        , ( "checkpoints", Encode.list encodeCheckpoint draft.checkpoints )
        , ( "date"
          , Maybe.map (DateOnly.toIsoString >> Encode.string) draft.date
                |> Maybe.withDefault Encode.null
          )
        , ( "time"
          , Maybe.map (Clock.toString >> Encode.string) draft.time
                |> Maybe.withDefault Encode.null
          )
        ]


encodeRoute : Route -> Encode.Value
encodeRoute course =
    Encode.object
        [ ( "samples"
          , Encode.list encodeSample
                (NonEmpty.toList (Route.points course)
                    |> List.map
                        (\point ->
                            { lat = point.position.lat
                            , lon = point.position.lon
                            , ele = Elevation.inMeters point.elevation
                            }
                        )
                )
          )
        , ( "waypoints", Encode.list encodeWaypoint (Route.waypoints course) )
        ]


encodeSample : Route.Sample -> Encode.Value
encodeSample sample =
    Encode.object
        [ ( "lat", Encode.float sample.lat )
        , ( "lon", Encode.float sample.lon )
        , ( "ele", Encode.float sample.ele )
        ]


encodeWaypoint : Route.Waypoint -> Encode.Value
encodeWaypoint waypoint =
    Encode.object
        [ ( "name", Encode.string waypoint.name )
        , ( "lat", Encode.float waypoint.position.lat )
        , ( "lon", Encode.float waypoint.position.lon )
        ]


encodeCheckpoint : Checkpoint -> Encode.Value
encodeCheckpoint checkpoint =
    Encode.object
        [ ( "id", Encode.string (Checkpoint.idToString checkpoint.id) )
        , ( "name", Encode.string checkpoint.name )
        , ( "km", Encode.float (Km.toFloat checkpoint.km) )
        , ( "cutoff"
          , case Checkpoint.cutoffClock checkpoint of
                Nothing ->
                    Encode.null

                Just clock ->
                    Encode.string (Clock.toString clock)
          )
        , ( "role", Encode.string (roleToString checkpoint.role) )
        ]


roleToString : Checkpoint.Role -> String
roleToString role =
    case role of
        Checkpoint.StartLine ->
            "start"

        Checkpoint.Station ->
            "station"

        Checkpoint.FinishLine ->
            "finish"


decoder : Decoder Draft
decoder =
    Decode.map4
        (\course checkpoints maybeDate maybeTime ->
            { route = course
            , checkpoints = checkpoints
            , date = maybeDate
            , time = maybeTime
            , nextId =
                1 + (List.maximum (List.map (.id >> Checkpoint.idToInt) checkpoints) |> Maybe.withDefault -1)
            }
        )
        (Decode.field "route" (Decode.nullable routeDecoder))
        (Decode.field "checkpoints" (Decode.list checkpointDecoder))
        (Decode.field "date" (Decode.nullable Decode.string)
            |> Decode.map (Maybe.andThen isoDate)
        )
        (Decode.field "time" (Decode.nullable Decode.string)
            |> Decode.map (Maybe.andThen Clock.fromString)
        )


isoDate : String -> Maybe DateOnly.DateOnly
isoDate text =
    case List.map String.toInt (String.split "-" text) of
        [ Just year, Just month, Just day ] ->
            DateOnly.fromParts year month day

        _ ->
            Nothing


routeDecoder : Decoder Route
routeDecoder =
    Decode.map2 Route.fromSamples
        (Decode.field "samples" (Decode.list sampleDecoder))
        (Decode.field "waypoints" (Decode.list waypointDecoder))
        |> Decode.andThen
            (\result ->
                case result of
                    Ok course ->
                        Decode.succeed course

                    Err error ->
                        Decode.fail (Route.errorMessage error)
            )


sampleDecoder : Decoder Route.Sample
sampleDecoder =
    Decode.map3 Route.Sample
        (Decode.field "lat" Decode.float)
        (Decode.field "lon" Decode.float)
        (Decode.oneOf [ Decode.field "ele" Decode.float, Decode.succeed 0 ])


waypointDecoder : Decoder Route.Waypoint
waypointDecoder =
    Decode.map2 (\name lat -> { name = name, position = lat })
        (Decode.oneOf [ Decode.field "name" Decode.string, Decode.succeed "" ])
        (Decode.map2 (\lat lon -> { lat = lat, lon = lon })
            (Decode.field "lat" Decode.float)
            (Decode.field "lon" Decode.float)
        )


checkpointDecoder : Decoder Checkpoint
checkpointDecoder =
    Decode.map5
        (\id name km cutoff role ->
            { id = Checkpoint.idFromInt id
            , name = name
            , km = Km.fromFloat km
            , cutoff = cutoff
            , role = role
            , status = Checkpoint.Pending
            }
        )
        (Decode.oneOf [ Decode.field "id" intFromAnything, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.field "name" Decode.string, Decode.succeed "" ])
        (Decode.field "km" Decode.float)
        (Decode.oneOf
            [ Decode.field "cutoff" (Decode.nullable Decode.string)
            , Decode.field "cot" (Decode.nullable Decode.string)
            , Decode.succeed Nothing
            ]
            |> Decode.map
                (\text ->
                    case Maybe.andThen Clock.fromString text of
                        Nothing ->
                            Checkpoint.NoCutoff

                        Just clock ->
                            Checkpoint.ClosesAt clock
                )
        )
        (Decode.oneOf [ Decode.field "role" Decode.string, Decode.succeed "station" ]
            |> Decode.map roleFromString
        )


roleFromString : String -> Checkpoint.Role
roleFromString text =
    case text of
        "start" ->
            Checkpoint.StartLine

        "finish" ->
            Checkpoint.FinishLine

        _ ->
            Checkpoint.Station


intFromAnything : Decoder Int
intFromAnything =
    Decode.oneOf
        [ Decode.int
        , Decode.string |> Decode.map (String.toInt >> Maybe.withDefault 0)
        ]
