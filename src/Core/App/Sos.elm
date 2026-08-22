module Core.App.Sos exposing (mapsLink, message)

{-| The message a lost runner sends to the organisers.

Three decisions are baked into the text and all three matter more than they
look.

Written without Vietnamese diacritics: an SMS containing them is encoded as
UCS-2 and each part carries 70 characters instead of 160, so this message would
split into three fragments, and in weak coverage a fragment goes missing.

It carries the nearest milestone on the course, because rescue teams know the
course and find a kilometre marker far faster than a coordinate.

It carries the accuracy, so whoever is searching knows what radius to sweep.

-}

import Core.App.Km as Km exposing (Km)
import Core.App.LatLon as LatLon
import Core.App.Progress exposing (Fix)
import Core.Data.DateOnly as DateOnly
import Core.Data.Distance as Distance
import Time


message : Time.Zone -> Fix -> Maybe Km -> String
message zone fix nearestMilestone =
    String.join " "
        [ "Toi bi lac va hien o vi tri"
        , LatLon.toCoordinateString fix.at
        , "vao luc"
        , timestamp zone fix.taken ++ "."
        , milestoneClause nearestMilestone
        , "Sai so GPS " ++ String.fromInt (Distance.inWholeMeters fix.accuracy) ++ " m."
        , "Ban do: " ++ mapsLink fix
        ]


milestoneClause : Maybe Km -> String
milestoneClause nearestMilestone =
    case nearestMilestone of
        Nothing ->
            ""

        Just milestone ->
            "Diem gan nhat tren duong chay la km " ++ Km.toString milestone ++ "."


mapsLink : Fix -> String
mapsLink fix =
    "https://maps.google.com/?q="
        ++ String.replace ", " "," (LatLon.toCoordinateString fix.at)


timestamp : Time.Zone -> Time.Posix -> String
timestamp zone moment =
    DateOnly.toString (DateOnly.fromPosix zone moment)
        ++ " "
        ++ pad (Time.toHour zone moment)
        ++ ":"
        ++ pad (Time.toMinute zone moment)


pad : Int -> String
pad n =
    String.padLeft 2 '0' (String.fromInt n)
