module Core.App.Plan exposing
    ( Draft
    , Issue(..)
    , Plan
    , ahead
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
import Core.Data.Duration as Duration
import Core.Data.NonEmpty as NonEmpty exposing (NonEmpty)
import Time


type Plan
    = Plan
        { route : Route
        , checkpoints : NonEmpty Checkpoint
        , date : DateOnly
        , time : Clock
        }


type alias Draft =
    { route : Maybe Route
    , checkpoints : List Checkpoint
    , date : Maybe DateOnly
    , time : Maybe Clock
    , nextId : Int
    }


{-| A blocking issue is an arithmetic contradiction. An advisory is a figure
that is possible but unusual enough to be worth a second look. Neither stops the
runner: some races really do publish odd cutoffs, and the app has no standing to
forbid anyone from running.
-}
type Issue
    = Blocking String
    | Advisory String


{-| Implied speeds outside this band mean a cutoff was almost certainly mistyped.
-}
implausiblyFastKmPerHour : Float
implausiblyFastKmPerHour =
    15


implausiblySlowKmPerHour : Float
implausiblySlowKmPerHour =
    0.7


emptyDraft : Draft
emptyDraft =
    { route = Nothing
    , checkpoints = []
    , date = Nothing
    , time = Nothing
    , nextId = 0
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

        added =
            (if hasRole Checkpoint.StartLine then
                []

             else
                [ Checkpoint.startLine (Checkpoint.idFromInt withRoute.nextId) startTime ]
            )
                ++ (if hasRole Checkpoint.FinishLine then
                        []

                    else
                        [ Checkpoint.finishLine
                            (Checkpoint.idFromInt (withRoute.nextId + 1))
                            (Route.totalKm newRoute)
                            NoCutoff
                        ]
                   )
    in
    { withRoute
        | checkpoints =
            (draft.checkpoints ++ added)
                |> List.map (pinToRoute newRoute)
        , nextId = withRoute.nextId + List.length added
    }


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


fromDraft : Draft -> Result (NonEmpty Issue) Plan
fromDraft draft =
    case ( draft.route, draft.date, draft.time ) of
        ( Just currentRoute, Just currentDate, Just currentTime ) ->
            case NonEmpty.fromList (Checkpoint.sortByKm draft.checkpoints) of
                Nothing ->
                    Err (NonEmpty.singleton (Blocking "Kế hoạch chưa có trạm nào."))

                Just list ->
                    Ok
                        (Plan
                            { route = currentRoute
                            , checkpoints = list
                            , date = currentDate
                            , time = currentTime
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


{-| A cutoff written on the race sheet, anchored to a real moment. Times before
the start roll forward a day, which is what makes an 01:00 cutoff on an evening
race land where a runner expects it.
-}
cutoffMoment : Time.Zone -> Plan -> Checkpoint -> Maybe Time.Posix
cutoffMoment zone plan checkpoint =
    Checkpoint.cutoffClock checkpoint
        |> Maybe.map
            (\clock ->
                let
                    began =
                        startsAt zone plan

                    sameDay =
                        DateOnly.at zone (date plan) clock
                in
                if Time.posixToMillis sameDay < Time.posixToMillis began - 60000 then
                    DateOnly.at zone (DateOnly.addDays 1 (date plan)) clock

                else
                    sameDay
            )


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
        , noCutoffsAtAll ordered
        ]


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


type alias Leg =
    { label : String, km : Km, moment : Time.Posix }


cutoffSequence : Time.Zone -> Plan -> List Checkpoint -> List Issue
cutoffSequence zone plan ordered =
    let
        began =
            startsAt zone plan

        timed =
            List.filter
                (\checkpoint ->
                    Checkpoint.hasCutoff checkpoint
                        && checkpoint.role
                        /= Checkpoint.StartLine
                        && (checkpoint.role /= Checkpoint.Station || Km.toFloat checkpoint.km > 0)
                )
                ordered

        step checkpoint ( previous, collected, index ) =
            case cutoffMoment zone plan checkpoint of
                Nothing ->
                    ( previous, collected, index )

                Just moment ->
                    let
                        found =
                            legIssues began previous checkpoint moment index
                    in
                    ( { label = "“" ++ Checkpoint.displayName checkpoint ++ "”"
                      , km = checkpoint.km
                      , moment = moment
                      }
                    , collected ++ found
                    , index + 1
                    )

        ( _, allIssues, _ ) =
            List.foldl step
                ( { label = "vạch xuất phát", km = Km.start, moment = began }, [], 0 )
                timed
    in
    allIssues


legIssues : Time.Posix -> Leg -> Checkpoint -> Time.Posix -> Int -> List Issue
legIssues began previous checkpoint moment index =
    let
        hours =
            Duration.inHours (Duration.between previous.moment moment)

        gapKm =
            Km.toFloat checkpoint.km - Km.toFloat previous.km

        startedTooLate =
            index == 0 && Duration.inHours (Duration.between began moment) > 18
    in
    if startedTooLate then
        [ Blocking
            ("Giờ xuất phát muộn hơn COT của “"
                ++ Checkpoint.displayName checkpoint
                ++ "”. Kiểm tra lại ngày giờ xuất phát."
            )
        ]

    else if hours <= 0 then
        [ Blocking
            ("COT của “"
                ++ Checkpoint.displayName checkpoint
                ++ "” không nằm sau "
                ++ previous.label
                ++ "."
            )
        ]

    else if gapKm > 0 && (gapKm / hours) > implausiblyFastKmPerHour then
        [ Advisory
            ("Từ "
                ++ previous.label
                ++ " tới “"
                ++ Checkpoint.displayName checkpoint
                ++ "”: "
                ++ Km.toString (Km.fromFloat gapKm)
                ++ " km mà chỉ cho "
                ++ Duration.toCompactString (Duration.fromMinutes (hours * 60))
                ++ " — tức hơn "
                ++ String.fromInt (round implausiblyFastKmPerHour)
                ++ " km/h. Kiểm tra lại COT."
            )
        ]

    else if gapKm > 0 && (gapKm / hours) < implausiblySlowKmPerHour then
        [ Advisory
            ("Từ "
                ++ previous.label
                ++ " tới “"
                ++ Checkpoint.displayName checkpoint
                ++ "”: "
                ++ Km.toString (Km.fromFloat gapKm)
                ++ " km mà cho tới "
                ++ Duration.toCompactString (Duration.fromMinutes (hours * 60))
                ++ " — có thể gõ nhầm giờ."
            )
        ]

    else
        []


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
