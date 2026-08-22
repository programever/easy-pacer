module Runtime.Gpx exposing (Error(..), Payload, decoder, errorMessage)

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


type Error
    = NotGpx
    | Malformed String


decoder : Decoder (Result Error Payload)
decoder =
    Decode.field "ok" Decode.bool
        |> Decode.andThen
            (\ok ->
                if ok then
                    Decode.map Ok payloadDecoder

                else
                    Decode.map (Err << Malformed)
                        (Decode.oneOf
                            [ Decode.field "error" Decode.string
                            , Decode.succeed "unknown"
                            ]
                        )
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
        NotGpx ->
            "Không đọc được file này — kiểm tra lại xem có đúng GPX không."

        Malformed _ ->
            "File GPX bị lỗi định dạng, app không đọc được."
