port module Runtime.Ports exposing
    ( requestGps, gpsIn
    , parseGpx, gpxIn
    , save, clear
    , copyText, openSms
    )

{-| The only place this program touches the outside world.

Ports carry DATA, never DECISIONS. The JavaScript on the other side reads a
sensor, parses XML, writes to storage or opens a messaging app; it never chooses
a milestone, never judges whether the runner is off course, never formats
anything a person reads. Everything arriving through an inbound port is an
untrusted `Value` and is decoded before it is allowed near the model.

Keeping this boundary thin is what makes Elm's no-runtime-exception guarantee
worth anything here: the guarantee stops at the port, so the port must not be
where the thinking happens.
-}

import Json.Encode as Encode


{-| Ask for one position. The JavaScript side listens for a few seconds and
returns the most accurate sample it saw, because a single reading under tree
cover can be tens of metres out.
-}
port requestGps : () -> Cmd msg


port gpsIn : (Encode.Value -> msg) -> Sub msg


{-| XML in, plain samples out. Parsing markup is a browser job; deciding what
the samples mean is `Core.App.Route`.
-}
port parseGpx : String -> Cmd msg


port gpxIn : (Encode.Value -> msg) -> Sub msg


port save : Encode.Value -> Cmd msg


port clear : () -> Cmd msg


port copyText : String -> Cmd msg


port openSms : { phone : String, body : String } -> Cmd msg
