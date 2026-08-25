module State exposing
    ( Dialog(..)
    , Field(..)
    , MapGesture
    , MapView
    , Model
    , Pixel
    , RaceState
    , Screen(..)
    , Scrub(..)
    , SetupState
    , Tab(..)
    , Toast
    , Typing
    , fitAll
    , fitTo
    , initialModel
    , noGesture
    , pointerDown
    , pointerMove
    , pointerUp
    , setup
    , zoomBy
    )

{-| Everything the program knows, in one value.

`Screen` is a sum type, so being in race mode without a course or a start time
is unrepresentable. That was three separate nullable fields before.

-}

import Core.App.Checkpoint as Checkpoint
import Core.App.Km exposing (Km)
import Core.App.LatLon exposing (Plane)
import Core.App.Plan as Plan exposing (Draft, Issue, Plan)
import Core.App.Position exposing (Candidate)
import Core.App.Progress exposing (Progress)
import Core.App.Route as Route exposing (Route)
import Core.Data.NonEmpty as NonEmpty exposing (NonEmpty)
import Dict exposing (Dict)
import Time


type alias Model =
    { zone : Time.Zone
    , now : Time.Posix
    , screen : Screen
    , sosPhone : String
    , toast : Maybe Toast
    , dialog : Maybe Dialog
    }


type Screen
    = Setting SetupState
    | Racing RaceState


{-| The setup screen: the draft being filled in, plus two things that are about
editing rather than about the plan and so do not belong on `Draft`.
-}
type alias SetupState =
    { draft : Draft
    , typing : Maybe Typing
    , scrub : Scrub
    }


{-| Which text box the runner is typing into. Start and finish have no
editable km, so only a station can carry `KmOf`.
-}
type Field
    = DateField
    | TimeField
    | KmOf Checkpoint.Id
    | CutoffOf Checkpoint.Id
    | TargetOf Checkpoint.Id
    | ClimbField


{-| The raw text in the box that has focus, kept separate from the parsed
value so that a half typed "053" is shown as "053" and not rewritten to
"00:53" under the runner's cursor. Dropped on blur, when the box redraws
from the parsed value and so tidies "0530" into "05:30".

Only one box has focus at a time, which is why this is a `Maybe` and not a
dictionary.

-}
type alias Typing =
    { field : Field
    , text : String
    , valid : Bool
    }


type alias RaceState =
    { plan : Plan
    , startedAt : Time.Posix
    , progress : Progress
    , tab : Tab
    , map : MapView
    , gesture : MapGesture
    , scrub : Scrub
    , kmEntryOpen : Bool
    , kmEntryText : String

    -- A GPS request is out and has not answered yet. Transient: not saved.
    , gpsPending : Bool
    }


type Tab
    = PlanTab
    | LocateTab


{-| Where the elevation cursor is. `NotScrubbing` is a state, not a null.
-}
type Scrub
    = NotScrubbing
    | ScrubbingAt Km


type Dialog
    = PlanReview (List Issue) Bool
    | PickPosition (NonEmpty Candidate)
    | ConfirmQuit
    | About


type alias Toast =
    { text : String, shownAt : Time.Posix }


{-| The map viewport: centre in projected kilometres, and how many SVG units one
kilometre occupies.
-}
type alias MapView =
    { centre : Plane
    , scale : Float
    }


{-| A point on the screen, in CSS pixels. Distinct from `Plane`, which is in
projected kilometres; mixing the two was the bug the type exists to prevent.
-}
type alias Pixel =
    { x : Float, y : Float }


{-| The fingers currently on the map. One finger pans; two pinch. Pointer ids
come from the browser and are only ever compared for equality.
-}
type alias MapGesture =
    { pointers : Dict Int Pixel
    , pinch : Maybe { distance : Float, scale : Float }
    }


canvasWidth : Float
canvasWidth =
    320


canvasHeight : Float
canvasHeight =
    300


padding : Float
padding =
    14


initialModel : Time.Zone -> Time.Posix -> Model
initialModel zone now =
    { zone = zone
    , now = now
    , screen = Setting (setup Plan.emptyDraft)
    , sosPhone = ""
    , toast = Nothing
    , dialog = Nothing
    }


setup : Draft -> SetupState
setup draft =
    { draft = draft, typing = Nothing, scrub = NotScrubbing }


noGesture : MapGesture
noGesture =
    { pointers = Dict.empty, pinch = Nothing }


fitAll : Route -> MapView
fitAll route =
    let
        planes =
            List.map .plane (planePoints route)

        xs =
            List.map .x planes

        ys =
            List.map .y planes

        low values =
            List.minimum values |> Maybe.withDefault 0

        high values =
            List.maximum values |> Maybe.withDefault 1

        spanX =
            Basics.max 0.05 (high xs - low xs)

        spanY =
            Basics.max 0.05 (high ys - low ys)
    in
    { centre =
        { x = (low xs + high xs) / 2
        , y = (low ys + high ys) / 2
        }
    , scale =
        Basics.min ((canvasWidth - padding * 2) / spanX)
            ((canvasHeight - padding * 2) / spanY)
    }


planePoints : Route -> List Route.Point
planePoints route =
    NonEmpty.toList (Route.points route)


{-| Centre on a point at roughly 900 metres across, the scale at which a runner
can see both themselves and the course line.
-}
fitTo : Plane -> MapView
fitTo centre =
    { centre = centre
    , scale = (canvasWidth - padding * 2) / 0.9
    }


{-| Fingers down. The second finger starts a pinch, measured from the distance
between the two at that moment.
-}
pointerDown : Int -> Pixel -> MapView -> MapGesture -> MapGesture
pointerDown id at view gesture =
    let
        pointers =
            Dict.insert id at gesture.pointers
    in
    { pointers = pointers
    , pinch =
        case Dict.values pointers of
            [ a, b ] ->
                Just { distance = pixelDistance a b, scale = view.scale }

            _ ->
                gesture.pinch
    }


{-| A finger moved. `unit` is how many SVG units one CSS pixel covers, which
the view knows and the model does not. A pointer that was never pressed on
the map is ignored rather than treated as a drag from nowhere.
-}
pointerMove : Route -> Float -> Int -> Pixel -> ( MapGesture, MapView ) -> ( MapGesture, MapView )
pointerMove route unit id at ( gesture, view ) =
    case Dict.get id gesture.pointers of
        Nothing ->
            ( gesture, view )

        Just previous ->
            let
                pointers =
                    Dict.insert id at gesture.pointers

                moved =
                    { gesture | pointers = pointers }
            in
            case ( Dict.values pointers, gesture.pinch ) of
                ( [ a, b ], Just pinch ) ->
                    if pinch.distance > 4 then
                        ( moved
                        , withScale route (pinch.scale * pixelDistance a b / pinch.distance) view
                        )

                    else
                        ( moved, view )

                ( [ _ ], _ ) ->
                    ( moved
                    , panBy ((at.x - previous.x) * unit) ((at.y - previous.y) * unit) view
                    )

                _ ->
                    ( moved, view )


pointerUp : Int -> MapGesture -> MapGesture
pointerUp id gesture =
    let
        pointers =
            Dict.remove id gesture.pointers
    in
    { pointers = pointers
    , pinch =
        if Dict.size pointers < 2 then
            Nothing

        else
            gesture.pinch
    }


pixelDistance : Pixel -> Pixel -> Float
pixelDistance a b =
    sqrt (((a.x - b.x) ^ 2) + ((a.y - b.y) ^ 2))


panBy : Float -> Float -> MapView -> MapView
panBy dx dy view =
    { view
        | centre =
            { x = view.centre.x - (dx / view.scale)
            , y = view.centre.y - (dy / view.scale)
            }
    }


zoomBy : Float -> Route -> MapView -> MapView
zoomBy factor route view =
    withScale route (view.scale * factor) view


withScale : Route -> Float -> MapView -> MapView
withScale route wanted view =
    { view | scale = clampScale route wanted }


clampScale : Route -> Float -> Float
clampScale route wanted =
    let
        planes =
            List.map .plane (planePoints route)

        spread values =
            case ( List.minimum values, List.maximum values ) of
                ( Just low, Just high ) ->
                    high - low

                _ ->
                    1

        span =
            Basics.max 0.05
                (Basics.max (spread (List.map .x planes)) (spread (List.map .y planes)))
    in
    clamp ((canvasWidth - padding * 2) / (span * 2.5)) (canvasWidth / 0.05) wanted
