module Core.App.Segment exposing
    ( CutoffMoment
    , Segment
    , deadline
    , toCheckpoint
    )

{-| The four numbers the race screen exists to show, for one checkpoint: how far
away it is, how much climbing and descending is left to reach it, and how long
until it closes.

Nothing here is predicted or judged. Distance and elevation are read off the
course; the remaining time is a clock subtraction. How fast to run is the
runner's business, and a wrong estimate is worse than none.

-}

import Core.App.Checkpoint exposing (Checkpoint)
import Core.App.Km as Km exposing (Km)
import Core.App.Plan as Plan exposing (Plan)
import Core.App.Route as Route
import Core.Data.Distance exposing (Distance)
import Core.Data.Duration as Duration exposing (Duration)
import Core.Data.Elevation exposing (Elevation)
import Time


type alias Segment =
    { distance : Distance
    , ascent : Elevation
    , descent : Elevation
    , cutoff : Maybe CutoffMoment
    }


type alias CutoffMoment =
    { closesAt : Time.Posix
    , remaining : Duration
    }


toCheckpoint : Time.Zone -> Plan -> Km -> Time.Posix -> Checkpoint -> Segment
toCheckpoint zone plan from now checkpoint =
    let
        course =
            Plan.route plan
    in
    { distance = Km.difference from checkpoint.km
    , ascent = Route.ascentBetween course from checkpoint.km
    , descent = Route.descentBetween course from checkpoint.km
    , cutoff =
        Plan.cutoffMoment zone plan checkpoint
            |> Maybe.map
                (\closesAt ->
                    { closesAt = closesAt
                    , remaining = Duration.between now closesAt
                    }
                )
    }


{-| The deadline that actually binds, and which checkpoint owns it.

When the next checkpoint is a water station it has no cutoff of its own, but the
deadline still exists further ahead. Leaving the countdown blank there wastes
the most important figure on the screen, so it is borrowed and labelled with the
checkpoint it belongs to.

-}
deadline : Time.Zone -> Plan -> Km -> Time.Posix -> Maybe ( Checkpoint, CutoffMoment )
deadline zone plan from now =
    Plan.nextWithCutoff from plan
        |> Maybe.andThen
            (\checkpoint ->
                (toCheckpoint zone plan from now checkpoint).cutoff
                    |> Maybe.map (\moment -> ( checkpoint, moment ))
            )
