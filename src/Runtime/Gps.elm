module Runtime.Gps exposing (Error, decoder, errorMessage)

{-| Decoding one position from the geolocation port.
-}

import Core.App.Progress exposing (Fix)
import Core.Data.Distance as Distance
import Json.Decode as Decode exposing (Decoder)
import Time


type Error
    = PermissionDenied
    | Unavailable
    | Timeout


decoder : Decoder (Result Error Fix)
decoder =
    Decode.field "ok" Decode.bool
        |> Decode.andThen
            (\ok ->
                if ok then
                    Decode.map Ok fixDecoder

                else
                    Decode.map (Err << codeToError) (Decode.field "code" Decode.int)
            )


fixDecoder : Decoder Fix
fixDecoder =
    Decode.map3
        (\lat lon rest ->
            { at = { lat = lat, lon = lon }
            , accuracy = rest.accuracy
            , taken = rest.taken
            }
        )
        (Decode.field "lat" Decode.float)
        (Decode.field "lon" Decode.float)
        (Decode.map2 (\accuracy taken -> { accuracy = accuracy, taken = taken })
            (Decode.oneOf
                [ Decode.field "accuracy" Decode.float |> Decode.map Distance.fromMeters
                , Decode.succeed (Distance.fromMeters 999)
                ]
            )
            (Decode.field "taken" Decode.int |> Decode.map Time.millisToPosix)
        )


codeToError : Int -> Error
codeToError code =
    case code of
        1 ->
            PermissionDenied

        3 ->
            Timeout

        _ ->
            Unavailable


{-| User facing.
-}
errorMessage : Error -> String
errorMessage error =
    case error of
        PermissionDenied ->
            "Bạn chưa cho phép truy cập vị trí."

        Unavailable ->
            "Không bắt được tín hiệu GPS."

        Timeout ->
            "Chờ GPS quá lâu, thử lại ở chỗ thoáng hơn."
