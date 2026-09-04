module Core.App.Plan exposing
    ( Draft
    , Issue
    , Plan
    , checkpoints
    , cutoffMoment
    , date
    , draftWithRoute
    , emptyDraft
    , fromDraft
    , isBlocking
    , issueText
    , issues
    , nextAhead
    , nextWithCutoff
    , route
    , startsAt
    , targetMoment
    , time
    , toDraft
    , withCheckpoints
    )

{-| The race sheet: a course, a list of checkpoints, and the moment the runner
leaves the start line.

`Draft` is what the setup screen holds while it is still being filled in;
`Plan` is what the race screen holds. `fromDraft` is the only way to obtain a
`Plan`, so entering race mode without a course or a start time is not a state
the program can reach. The plan review is not advice any more, it is the
constructor.

-}

import Core.App.Checkpoint as Checkpoint exposing (Checkpoint, Cutoff(..))
import Core.App.Km as Km exposing (Km)
import Core.App.Route as Route exposing (Route)
import Core.Data.Clock as Clock exposing (Clock)
import Core.Data.DateOnly as DateOnly exposing (DateOnly)
import Core.Data.NonEmpty as NonEmpty exposing (NonEmpty)
import Time


type Plan
    = Plan
        { route : Route
        , checkpoints : NonEmpty Checkpoint
        , date : DateOnly
        , time : Clock
        , name : String
        }


{-| `name` is the name of the file the plan came from, without its extension,
and is what the plan is saved as. Empty until a file has been loaded.
-}
type alias Draft =
    { route : Maybe Route
    , checkpoints : List Checkpoint
    , date : Maybe DateOnly
    , time : Maybe Clock
    , nextId : Int
    , name : String
    }


{-| A blocking issue is an arithmetic contradiction. An advisory is a figure
that is possible but unusual enough to be worth a second look. Neither stops the
runner: some races really do publish odd cutoffs, and the app has no standing to
forbid anyone from running.
-}
type Issue
    = Blocking String
    | Advisory String


emptyDraft : Draft
emptyDraft =
    { route = Nothing
    , checkpoints = []
    , date = Nothing
    , time = Nothing
    , nextId = 0
    , name = ""
    }


{-| Attaching a course fixes the finish line's position, and creates the start
and finish rows if they are not there yet.
-}
draftWithRoute : Route -> Draft -> Draft
draftWithRoute newRoute draft =
    let
        withRoute =
            { draft | route = Just newRoute }

        hasRole role =
            List.any (\checkpoint -> checkpoint.role == role) draft.checkpoints

        startTime =
            Maybe.withDefault Clock.midnight draft.time

        addedStart =
            if hasRole Checkpoint.StartLine then
                []

            else
                [ Checkpoint.startLine (Checkpoint.idFromInt withRoute.nextId) startTime ]

        addedFinish =
            if hasRole Checkpoint.FinishLine then
                []

            else
                [ Checkpoint.finishLine
                    (Checkpoint.idFromInt (withRoute.nextId + List.length addedStart))
                    (Route.totalKm newRoute)
                    NoCutoff
                ]
    in
    { withRoute
        | checkpoints =
            (addedStart ++ draft.checkpoints ++ addedFinish)
                |> List.map (pinToRoute newRoute)
        , nextId = withRoute.nextId + List.length addedStart + List.length addedFinish
    }


{-| The start line "closes" the moment the gun goes, whatever the draft's
stored copy says: the start cutoff was written down when the course was
loaded, and the runner may have set the real start time afterwards. It is the
anchor of the whole timeline, so it is pinned here, at the only door into a
`Plan`.
-}
pinStartCutoff : Clock -> Checkpoint -> Checkpoint
pinStartCutoff startTime checkpoint =
    case checkpoint.role of
        Checkpoint.StartLine ->
            { checkpoint | cutoff = ClosesAt startTime }

        Checkpoint.Station ->
            checkpoint

        Checkpoint.FinishLine ->
            checkpoint


{-| Start and finish positions come from the course, never from typing.
-}
pinToRoute : Route -> Checkpoint -> Checkpoint
pinToRoute currentRoute checkpoint =
    case checkpoint.role of
        Checkpoint.StartLine ->
            { checkpoint | km = Km.start }

        Checkpoint.FinishLine ->
            { checkpoint | km = Route.totalKm currentRoute }

        Checkpoint.Station ->
            checkpoint


{-| The checkpoints keep the order the runner arranged them in. Anything in
race mode that needs the physical course order sorts by km itself.
-}
fromDraft : Draft -> Result (NonEmpty Issue) Plan
fromDraft draft =
    case ( draft.route, draft.date, draft.time ) of
        ( Just currentRoute, Just currentDate, Just currentTime ) ->
            case NonEmpty.fromList draft.checkpoints of
                Nothing ->
                    Err (NonEmpty.singleton (Blocking "Kế hoạch chưa có trạm nào."))

                Just list ->
                    Ok
                        (Plan
                            { route = currentRoute
                            , checkpoints = NonEmpty.map (pinStartCutoff currentTime) list
                            , date = currentDate
                            , time = currentTime
                            , name = draft.name
                            }
                        )

        _ ->
            Err (NonEmpty.singleton (Blocking (missingText draft)))


missingText : Draft -> String
missingText draft =
    let
        missing =
            List.filterMap identity
                [ whenNothing draft.route "chưa nạp file GPX"
                , whenNothing draft.date "chưa có ngày xuất phát"
                , whenNothing draft.time "chưa có giờ xuất phát"
                ]
    in
    "Còn thiếu: " ++ String.join ", " missing ++ "."


whenNothing : Maybe a -> String -> Maybe String
whenNothing value text =
    case value of
        Just _ ->
            Nothing

        Nothing ->
            Just text


toDraft : Plan -> Int -> Draft
toDraft (Plan plan) nextId =
    { route = Just plan.route
    , checkpoints = NonEmpty.toList plan.checkpoints
    , date = Just plan.date
    , time = Just plan.time
    , nextId = nextId
    , name = plan.name
    }


route : Plan -> Route
route (Plan plan) =
    plan.route


checkpoints : Plan -> NonEmpty Checkpoint
checkpoints (Plan plan) =
    plan.checkpoints


date : Plan -> DateOnly
date (Plan plan) =
    plan.date


time : Plan -> Clock
time (Plan plan) =
    plan.time


withCheckpoints : NonEmpty Checkpoint -> Plan -> Plan
withCheckpoints updated (Plan plan) =
    Plan { plan | checkpoints = updated }


startsAt : Time.Zone -> Plan -> Time.Posix
startsAt zone (Plan plan) =
    DateOnly.at zone plan.date plan.time


{-| A cutoff written on the race sheet, anchored to a real moment.

A wall clock names a time of day, not a day, so each cutoff is anchored to
the one before it on the course: the first occurrence of its clock time at or
after the previous cutoff, starting from the start line. Midnight can pass
under the chain any number of times, so a race that starts at 22:00 or runs
into a second day still reads in order — provided no two consecutive cutoffs
are more than a day apart, which no organiser's sheet does.

-}
cutoffMoment : Time.Zone -> Plan -> Checkpoint -> Maybe Time.Posix
cutoffMoment zone plan checkpoint =
    momentInChain zone plan Checkpoint.cutoffClock checkpoint


{-| The runner's own target for a checkpoint. A target belongs to the same
day as the station's deadline, so where a cutoff exists the target is the
occurrence of its clock time nearest to that cutoff — which is what lets a
target run past the cutoff and be caught, instead of sliding a day forward.
A station with no cutoff anchors to the nearest occurrence from the target
before it, so a backwards clock reads as backwards and the review flags it.
-}
targetMoment : Time.Zone -> Plan -> Checkpoint -> Maybe Time.Posix
targetMoment zone plan checkpoint =
    checkpoint.target
        |> Maybe.andThen
            (\_ ->
                let
                    step current ( previous, found ) =
                        case ( found, current.target ) of
                            ( Just _, _ ) ->
                                ( previous, found )

                            ( Nothing, Nothing ) ->
                                ( previous, Nothing )

                            ( Nothing, Just clock ) ->
                                let
                                    moment =
                                        case momentInChain zone plan Checkpoint.cutoffClock current of
                                            Just closesAt ->
                                                nearestTo zone closesAt clock

                                            Nothing ->
                                                nearestTo zone previous clock
                                in
                                ( moment
                                , if current.id == checkpoint.id then
                                    Just moment

                                  else
                                    Nothing
                                )
                in
                Checkpoint.sortByKm (NonEmpty.toList (checkpoints plan))
                    |> List.foldl step ( startsAt zone plan, Nothing )
                    |> Tuple.second
            )


momentInChain : Time.Zone -> Plan -> (Checkpoint -> Maybe Clock) -> Checkpoint -> Maybe Time.Posix
momentInChain zone plan clockOf checkpoint =
    clockOf checkpoint
        |> Maybe.andThen
            (\_ ->
                let
                    step current ( previous, found ) =
                        case ( found, clockOf current ) of
                            ( Just _, _ ) ->
                                ( previous, found )

                            ( Nothing, Nothing ) ->
                                ( previous, Nothing )

                            ( Nothing, Just clock ) ->
                                let
                                    moment =
                                        firstAtOrAfter zone previous clock
                                in
                                ( moment
                                , if current.id == checkpoint.id then
                                    Just moment

                                  else
                                    Nothing
                                )
                in
                Checkpoint.sortByKm (NonEmpty.toList (checkpoints plan))
                    |> List.foldl step ( startsAt zone plan, Nothing )
                    |> Tuple.second
            )


{-| The first moment showing this clock time at or after the anchor, with a
minute of slack so a cutoff equal to the anchor does not jump a day.
-}
firstAtOrAfter : Time.Zone -> Time.Posix -> Clock -> Time.Posix
firstAtOrAfter zone anchor clock =
    let
        sameDay =
            DateOnly.at zone (DateOnly.fromPosix zone anchor) clock
    in
    if Time.posixToMillis sameDay < Time.posixToMillis anchor - 60000 then
        DateOnly.at zone (DateOnly.addDays 1 (DateOnly.fromPosix zone anchor)) clock

    else
        sameDay


{-| The occurrence of this clock time closest to the anchor; on a tie the
earlier one, which is the charitable reading of a target against a cutoff.
-}
nearestTo : Time.Zone -> Time.Posix -> Clock -> Time.Posix
nearestTo zone anchor clock =
    let
        day =
            DateOnly.fromPosix zone anchor

        dayBefore =
            DateOnly.fromPosix zone (Time.millisToPosix (Time.posixToMillis anchor - 86400000))

        distanceTo candidate =
            abs (Time.posixToMillis candidate - Time.posixToMillis anchor)
    in
    [ DateOnly.at zone dayBefore clock
    , DateOnly.at zone day clock
    , DateOnly.at zone (DateOnly.addDays 1 day) clock
    ]
        |> List.sortBy distanceTo
        |> List.head
        |> Maybe.withDefault anchor


ahead : Km -> Plan -> List Checkpoint
ahead reached plan =
    NonEmpty.toList (checkpoints plan)
        |> List.filter (\checkpoint -> Checkpoint.isPending checkpoint && not (Km.isAtOrBefore reached checkpoint.km))
        |> Checkpoint.sortByKm


nextAhead : Km -> Plan -> Maybe Checkpoint
nextAhead reached plan =
    List.head (ahead reached plan)


{-| The nearest checkpoint ahead that actually closes. A water station has no
cutoff of its own, but the deadline still exists somewhere in front, and the
race screen has to be able to name it.
-}
nextWithCutoff : Km -> Plan -> Maybe Checkpoint
nextWithCutoff reached plan =
    List.filter Checkpoint.hasCutoff (ahead reached plan)
        |> List.head


issueText : Issue -> String
issueText issue =
    case issue of
        Blocking text ->
            text

        Advisory text ->
            text


isBlocking : Issue -> Bool
isBlocking issue =
    case issue of
        Blocking _ ->
            True

        Advisory _ ->
            False


{-| Everything a machine can check about a race sheet without knowing the race.

The whole app rests on cutoffs typed in by hand at a kitchen table. One wrong
digit makes every figure downstream wrong, and the runner finds out at two in
the morning in the middle of a forest.

-}
issues : Time.Zone -> Plan -> List Issue
issues zone plan =
    let
        ordered =
            Checkpoint.sortByKm (NonEmpty.toList (checkpoints plan))
    in
    List.concat
        [ beyondCourse plan ordered
        , duplicatePositions ordered
        , cutoffSequence zone plan ordered
        , targetSequence zone plan ordered
        , targetBeyondCutoff zone plan ordered
        , noCutoffsAtAll ordered
        ]


{-| Aiming to arrive after the station has closed is aiming to be pulled from
the race. Arriving exactly at the cutoff is allowed: tight, but a plan.
-}
targetBeyondCutoff : Time.Zone -> Plan -> List Checkpoint -> List Issue
targetBeyondCutoff zone plan ordered =
    List.filterMap
        (\checkpoint ->
            Maybe.map2
                (\targetAt closesAt ->
                    if Time.posixToMillis targetAt > Time.posixToMillis closesAt then
                        Just
                            (Blocking
                                ("Mục tiêu của “"
                                    ++ Checkpoint.displayName checkpoint
                                    ++ "” muộn hơn COT của chính trạm này."
                                )
                            )

                    else
                        Nothing
                )
                (targetMoment zone plan checkpoint)
                (cutoffMoment zone plan checkpoint)
                |> Maybe.andThen identity
        )
        ordered


{-| Targets follow the course: a checkpoint further along must have a later
target than the one before it, or one of the two is a typo.
-}
targetSequence : Time.Zone -> Plan -> List Checkpoint -> List Issue
targetSequence zone plan ordered =
    let
        timed =
            List.filterMap
                (\checkpoint ->
                    Maybe.map (Tuple.pair checkpoint) (targetMoment zone plan checkpoint)
                )
                ordered
    in
    List.map2 Tuple.pair timed (List.drop 1 timed)
        |> List.filterMap
            (\( ( earlier, earlierAt ), ( later, laterAt ) ) ->
                if Time.posixToMillis laterAt <= Time.posixToMillis earlierAt then
                    Just
                        (Blocking
                            ("Mục tiêu của “"
                                ++ Checkpoint.displayName later
                                ++ "” không nằm sau mục tiêu của “"
                                ++ Checkpoint.displayName earlier
                                ++ "”."
                            )
                        )

                else
                    Nothing
            )


beyondCourse : Plan -> List Checkpoint -> List Issue
beyondCourse plan ordered =
    let
        courseLength =
            Km.toFloat (Route.totalKm (route plan))
    in
    List.filterMap
        (\checkpoint ->
            if checkpoint.role == Checkpoint.Station && Km.toFloat checkpoint.km > courseLength + 0.1 then
                Just
                    (Blocking
                        ("“"
                            ++ Checkpoint.displayName checkpoint
                            ++ "” đặt ở km "
                            ++ Km.toString checkpoint.km
                            ++ " nhưng đường chạy chỉ dài "
                            ++ Km.toString (Route.totalKm (route plan))
                            ++ " km — có thể bạn nạp nhầm file GPX của cự ly khác."
                        )
                    )

            else if checkpoint.role == Checkpoint.Station && Km.toFloat checkpoint.km <= 0 then
                Just
                    (Blocking
                        ("“" ++ Checkpoint.displayName checkpoint ++ "” chưa có số km.")
                    )

            else
                Nothing
        )
        ordered


duplicatePositions : List Checkpoint -> List Issue
duplicatePositions ordered =
    List.map2 Tuple.pair ordered (List.drop 1 ordered)
        |> List.filterMap
            (\( earlier, later ) ->
                if
                    abs (Km.toFloat later.km - Km.toFloat earlier.km)
                        < 0.05
                        && Km.toFloat later.km
                        > 0
                then
                    Just
                        (Advisory
                            ("“"
                                ++ Checkpoint.displayName earlier
                                ++ "” và “"
                                ++ Checkpoint.displayName later
                                ++ "” cùng ở km "
                                ++ Km.toString later.km
                                ++ "."
                            )
                        )

                else
                    Nothing
            )


{-| The start is the anchor of the timeline; everything after it is the
organiser's business, however long the race runs. The only thing left to
check is arithmetic: with each cutoff anchored to the one before it, a
cutoff can only land earlier than its predecessor through a typo inside the
one-minute slack. Two stations closing at the same moment are allowed.
-}
cutoffSequence : Time.Zone -> Plan -> List Checkpoint -> List Issue
cutoffSequence zone plan ordered =
    let
        timed =
            List.filterMap
                (\checkpoint ->
                    if
                        (checkpoint.role /= Checkpoint.StartLine)
                            && (checkpoint.role /= Checkpoint.Station || Km.toFloat checkpoint.km > 0)
                    then
                        Maybe.map (Tuple.pair checkpoint) (cutoffMoment zone plan checkpoint)

                    else
                        Nothing
                )
                ordered
    in
    List.map2 Tuple.pair timed (List.drop 1 timed)
        |> List.filterMap
            (\( ( earlier, earlierAt ), ( later, laterAt ) ) ->
                if Time.posixToMillis laterAt < Time.posixToMillis earlierAt then
                    Just
                        (Blocking
                            ("COT của “"
                                ++ Checkpoint.displayName later
                                ++ "” không nằm sau COT của “"
                                ++ Checkpoint.displayName earlier
                                ++ "”."
                            )
                        )

                else
                    Nothing
            )


noCutoffsAtAll : List Checkpoint -> List Issue
noCutoffsAtAll ordered =
    if
        List.any
            (\checkpoint -> Checkpoint.hasCutoff checkpoint && checkpoint.role /= Checkpoint.StartLine)
            ordered
    then
        []

    else
        [ Advisory "Chưa trạm nào có giờ đóng trạm — app sẽ chỉ hiện quãng đường và độ cao." ]
