module Core.App.Checkpoint exposing
    ( Checkpoint
    , Cutoff(..)
    , Id
    , Role(..)
    , Status(..)
    , cutoffClock
    , displayName
    , finishLine
    , hasCutoff
    , idFromInt
    , idToInt
    , idToString
    , isPassed
    , isPending
    , moveDown
    , moveUp
    , passedAt
    , sortByKm
    , startLine
    , station
    , syncStatus
    )

{-| One point on the race sheet: a checkpoint, a water station, the start line
or the finish.

Two shapes here exist because of bugs the previous version actually had.
`Cutoff` is a sum type, so an empty string can no longer mean "no cutoff" by
accident. `Status` carries the arrival time inside the `Passed` variant, so
there is no nullable timestamp to leave stale.

-}

import Core.App.Km as Km exposing (Km)
import Core.Data.Clock exposing (Clock)
import Time


type Id
    = Id Int


{-| `cutoff` is the organiser's deadline; `target` is the runner's own plan
for the same clock, optional and theirs to break.
-}
type alias Checkpoint =
    { id : Id
    , name : String
    , km : Km
    , cutoff : Cutoff
    , target : Maybe Clock
    , role : Role
    , status : Status
    }


{-| The start and finish are fixed: they cannot be deleted or reordered, and
their km positions are dictated by the route rather than typed in.
-}
type Role
    = StartLine
    | Station
    | FinishLine


type Cutoff
    = NoCutoff
    | ClosesAt Clock


type Status
    = Pending
    | Passed Time.Posix


idFromInt : Int -> Id
idFromInt =
    Id


idToInt : Id -> Int
idToInt (Id value) =
    value


idToString : Id -> String
idToString (Id value) =
    String.fromInt value


startLine : Id -> Clock -> Checkpoint
startLine id time =
    { id = id
    , name = "Xuất phát"
    , km = Km.start
    , cutoff = ClosesAt time
    , target = Nothing
    , role = StartLine
    , status = Pending
    }


finishLine : Id -> Km -> Cutoff -> Checkpoint
finishLine id km cutoff =
    { id = id
    , name = "Về đích"
    , km = km
    , cutoff = cutoff
    , target = Nothing
    , role = FinishLine
    , status = Pending
    }


station : Id -> String -> Km -> Cutoff -> Checkpoint
station id name km cutoff =
    { id = id
    , name = name
    , km = km
    , cutoff = cutoff
    , target = Nothing
    , role = Station
    , status = Pending
    }


{-| User facing. A station the runner has not named yet still needs something to
show in a list.
-}
displayName : Checkpoint -> String
displayName checkpoint =
    if String.trim checkpoint.name /= "" then
        checkpoint.name

    else
        case checkpoint.role of
            StartLine ->
                "Xuất phát"

            FinishLine ->
                "Về đích"

            Station ->
                "Trạm chưa đặt tên"


isPending : Checkpoint -> Bool
isPending checkpoint =
    case checkpoint.status of
        Pending ->
            True

        Passed _ ->
            False


isPassed : Checkpoint -> Bool
isPassed checkpoint =
    not (isPending checkpoint)


passedAt : Checkpoint -> Maybe Time.Posix
passedAt checkpoint =
    case checkpoint.status of
        Pending ->
            Nothing

        Passed moment ->
            Just moment


hasCutoff : Checkpoint -> Bool
hasCutoff checkpoint =
    case checkpoint.cutoff of
        NoCutoff ->
            False

        ClosesAt _ ->
            True


cutoffClock : Checkpoint -> Maybe Clock
cutoffClock checkpoint =
    case checkpoint.cutoff of
        NoCutoff ->
            Nothing

        ClosesAt clock ->
            Just clock


sortByKm : List Checkpoint -> List Checkpoint
sortByKm =
    List.sortBy (\checkpoint -> Km.toFloat checkpoint.km)


{-| Swap a station with the one just before it in the list. Only stations
move: the start and finish never move, and a station never crosses them.
An id at the top, or one that is not a station, leaves the list untouched.
-}
moveUp : Id -> List Checkpoint -> List Checkpoint
moveUp id checkpoints =
    case checkpoints of
        first :: second :: rest ->
            if second.id == id && first.role == Station && second.role == Station then
                second :: first :: rest

            else
                first :: moveUp id (second :: rest)

        _ ->
            checkpoints


moveDown : Id -> List Checkpoint -> List Checkpoint
moveDown id checkpoints =
    case checkpoints of
        first :: second :: rest ->
            if first.id == id && first.role == Station && second.role == Station then
                second :: first :: rest

            else
                first :: moveDown id (second :: rest)

        _ ->
            checkpoints


{-| Status is derived from progress in BOTH directions. Correcting the distance
downwards has to un-pass the checkpoints beyond it; the previous version only
ever marked forwards, so a mistyped number left phantom passed stations behind
and every figure after them was quietly wrong.
-}
syncStatus : Km -> Time.Posix -> Checkpoint -> Checkpoint
syncStatus reached now checkpoint =
    case ( Km.isAtOrBefore reached checkpoint.km, checkpoint.status ) of
        ( True, Pending ) ->
            { checkpoint | status = Passed now }

        ( False, Passed _ ) ->
            { checkpoint | status = Pending }

        _ ->
            checkpoint
