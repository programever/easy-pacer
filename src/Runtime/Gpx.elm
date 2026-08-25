module Runtime.Gpx exposing (Error, Payload, decoder, errorMessage)

{-| Decoding what the GPX port sends back. The browser did the XML; this decides
whether what came back is usable.
-}

import Core.App.Route as Route
import Json.Decode as Decode exposing (Decoder)


type alias Payload =
    { name : String
    , samples : List Route.Sample
    , waypoints : List Route.Waypoint
    }


{-| The port's error text stays on the JS side of the fence: the user-facing
message is decided here and does not repeat browser internals.
-}
type Error
    = Malformed


decoder : Decoder (Result Error Payload)
decoder =
    Decode.field "ok" Decode.bool
        |> Decode.andThen
            (\ok ->
                if ok then
                    Decode.map Ok payloadDecoder

                else
                    Decode.succeed (Err Malformed)
            )


payloadDecoder : Decoder Payload
payloadDecoder =
    Decode.map3 Payload
        (Decode.oneOf [ Decode.field "name" Decode.string, Decode.succeed "" ])
        (Decode.field "samples" (Decode.list sampleDecoder))
        (Decode.oneOf
            [ Decode.field "waypoints" (Decode.list waypointDecoder)
            , Decode.succeed []
            ]
        )


sampleDecoder : Decoder Route.Sample
sampleDecoder =
    Decode.map3 Route.Sample
        (Decode.field "lat" Decode.float)
        (Decode.field "lon" Decode.float)
        (Decode.oneOf [ Decode.field "ele" Decode.float, Decode.succeed 0 ])


waypointDecoder : Decoder Route.Waypoint
waypointDecoder =
    Decode.map2 (\name position -> { name = name, position = position })
        (Decode.oneOf [ Decode.field "name" Decode.string, Decode.succeed "" ])
        (Decode.map2 (\lat lon -> { lat = lat, lon = lon })
            (Decode.field "lat" Decode.float)
            (Decode.field "lon" Decode.float)
        )


{-| User facing.
-}
errorMessage : Error -> String
errorMessage error =
    case error of
        Malformed ->
            "File GPX bị lỗi định dạng, app không đọc được."
