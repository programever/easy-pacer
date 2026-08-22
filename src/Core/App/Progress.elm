module Core.App.Progress exposing
    ( Fix
    , Progress
    , Source(..)
    , Stored
    , age
    , atStart
    , averageSpeedKmPerMinute
    , breadcrumbs
    , fromGps
    , fromRunner
    , fromStored
    , km
    , lastFix
    , lastOnRoute
    , source
    , toStored
    , updatedAt
    )

{-| How far the runner has got, when that was last established, and how.

`Source` replaces a boolean `manual` flag. A position typed in by the runner
cannot carry a GPS accuracy, and a GPS position cannot pretend to be a
declaration; neither state is representable now.

-}

import Core.App.Km as Km exposing (Km)
import Core.App.LatLon exposing (LatLon)
import Core.Data.Distance exposing (Distance)
import Core.Data.Duration as Duration exposing (Duration)
import Time


type alias Fix =
    { at : LatLon
    , accuracy : Distance
    , taken : Time.Posix
    }


type Source
    = FromGps Fix
    | FromRunner


type Progress
    = Progress
        { km : Km
        , updatedAt : Time.Posix
        , source : Source
        , breadcrumbs : List Fix
        , lastOnRoute : Maybe LatLon
        }


{-| The persisted shape. It is the whole value, but as a plain record, so that
`Storage.Snapshot` can write it out and read it back without `Progress` having
to expose its constructor to everyone else.
-}
type alias Stored =
    { km : Km
    , updatedAt : Time.Posix
    , source : Source
    , breadcrumbs : List Fix
    , lastOnRoute : Maybe LatLon
    }


toStored : Progress -> Stored
toStored (Progress state) =
    state


fromStored : Stored -> Progress
fromStored =
    Progress


maxBreadcrumbs : Int
maxBreadcrumbs =
    60


atStart : Time.Posix -> Progress
atStart moment =
    Progress
        { km = Km.start
        , updatedAt = moment
        , source = FromRunner
        , breadcrumbs = []
        , lastOnRoute = Nothing
        }


{-| The runner typing a distance is a decision, not a measurement, and it
overrides whatever the last fix said.
-}
fromRunner : Km -> Time.Posix -> Progress -> Progress
fromRunner reached now (Progress state) =
    Progress
        { state
            | km = reached
            , updatedAt = now
            , source = FromRunner
        }


fromGps : Km -> Fix -> Bool -> Progress -> Progress
fromGps reached fix onRoute (Progress state) =
    Progress
        { state
            | km = reached
            , updatedAt = fix.taken
            , source = FromGps fix
            , breadcrumbs = List.take maxBreadcrumbs (fix :: state.breadcrumbs)
            , lastOnRoute =
                if onRoute then
                    Just fix.at

                else
                    state.lastOnRoute
        }


km : Progress -> Km
km (Progress state) =
    state.km


updatedAt : Progress -> Time.Posix
updatedAt (Progress state) =
    state.updatedAt


source : Progress -> Source
source (Progress state) =
    state.source


breadcrumbs : Progress -> List Fix
breadcrumbs (Progress state) =
    state.breadcrumbs


lastFix : Progress -> Maybe Fix
lastFix (Progress state) =
    case state.source of
        FromGps fix ->
            Just fix

        FromRunner ->
            List.head state.breadcrumbs


lastOnRoute : Progress -> Maybe LatLon
lastOnRoute (Progress state) =
    state.lastOnRoute


{-| Measured, never modelled: distance covered divided by time elapsed. Used
only to break ties between candidate positions on an out-and-back section.
-}
averageSpeedKmPerMinute : Time.Posix -> Progress -> Maybe Float
averageSpeedKmPerMinute startedAt (Progress state) =
    let
        minutes =
            Duration.inMinutes (Duration.between startedAt state.updatedAt)
    in
    if minutes > 0 && Km.toFloat state.km > 0.5 then
        Just (Km.toFloat state.km / minutes)

    else
        Nothing


age : Time.Posix -> Progress -> Duration
age now (Progress state) =
    Duration.between state.updatedAt now
