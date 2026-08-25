module Core.App.Segment exposing
    ( CutoffMoment
    , Segment
    , Urgency(..)
    , deadline
    , requiredPace
    , toCheckpoint
    , urgency
    )

{-| The four numbers the race screen exists to show, for one checkpoint: how far
away it is, how much climbing and descending is left to reach it, and how long
until it closes.

Nothing here is predicted. Distance and elevation are read off the course;
the remaining time is a clock subtraction. How fast to run is the runner's
business, and a wrong estimate is worse than none.

-}

import Core.App.Checkpoint exposing (Checkpoint)
import Core.App.Km as Km exposing (Km)
import Core.App.Plan as Plan exposing (Plan)
import Core.App.Route as Route
import Core.Data.Distance as Distance exposing (Distance)
import Core.Data.Duration as Duration exposing (Duration)
import Core.Data.Elevation as Elevation exposing (Elevation)
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


{-| How much attention a countdown deserves. Derived, never stored.
-}
type Urgency
    = NoDeadline
    | Comfortable
    | Tight
    | Critical
    | Missed


{-| The colour is decided by the pace the cutoff demands, not by the runner's
own pace, which this app refuses to predict. Slower than `relaxedPace` per
flat-equivalent km and the budget is generous; faster than `dangerPace` and a
tired runner on a rocky mountain course is in real trouble.
-}
relaxedPace : Float
relaxedPace =
    15


dangerPace : Float
dangerPace =
    10


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


urgency : Int -> Segment -> Urgency
urgency climbRatio segment =
    case segment.cutoff of
        Nothing ->
            NoDeadline

        Just moment ->
            if Duration.isNegative moment.remaining then
                Missed

            else
                case requiredPace climbRatio segment of
                    Nothing ->
                        Comfortable

                    Just pace ->
                        if pace > relaxedPace then
                            Comfortable

                        else if pace >= dangerPace then
                            Tight

                        else
                            Critical


{-| Minutes per flat-equivalent km that the remaining budget allows: the time
until the cutoff divided by the distance with the climb exchanged for flat
metres at `climbRatio` metres per 100 m of ascent. Arithmetic on the clock and
the course, not a prediction. `Nothing` when there is no cutoff or the runner
is already at the checkpoint.
-}
requiredPace : Int -> Segment -> Maybe Float
requiredPace climbRatio segment =
    segment.cutoff
        |> Maybe.andThen
            (\moment ->
                let
                    effectiveKm =
                        Distance.inKilometers segment.distance
                            + Elevation.inMeters segment.ascent
                            * Basics.toFloat climbRatio
                            / 100000
                in
                if effectiveKm < 0.01 then
                    Nothing

                else
                    Just (Duration.inMinutes moment.remaining / effectiveKm)
            )


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
