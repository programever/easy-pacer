module View.Map exposing (Config, You(..), gestureHint, unitsPerPixel, view)

{-| The course drawn straight from the GPX, with no map tiles and no library.

Tiles need a network, and the moment a runner most needs a map is the moment
they are least likely to have one. Everything here is computed from data already
in memory, so the map works with the phone in flight mode.

-}

import Core.App.Checkpoint as Checkpoint exposing (Checkpoint)
import Core.App.Km as Km exposing (Km)
import Core.App.LatLon as LatLon exposing (LatLon, Plane)
import Core.App.Progress exposing (Fix)
import Core.App.Route as Route exposing (Route)
import Core.Data.Distance as Distance
import Core.Data.NonEmpty as NonEmpty
import Html exposing (Attribute)
import State exposing (MapView)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import View.Theme as Theme


type alias Config =
    { route : Route
    , view : MapView
    , checkpoints : List Checkpoint
    , reached : Km
    , you : You
    , lost : Bool
    , uncertain : Bool
    , snapTo : Maybe LatLon
    , cursor : Maybe Km
    , breadcrumbs : List LatLon
    }


{-| Where the runner is, and on what authority. A GPS fix knows a latitude and
an accuracy; a distance the runner typed knows neither, so it is drawn on the
course at the km they claimed. `Maybe Fix` said only "no dot yet", which left a
runner without GPS with no dot at all, and a runner who typed a km after a fix
with a dot still sitting at the old fix.
-}
type You
    = Measured Fix
    | Declared


width : Float
width =
    320


height : Float
height =
    300


view : List (Attribute msg) -> Config -> Svg msg
view attributes config =
    let
        toScreen plane =
            { x = width / 2 + ((plane.x - config.view.centre.x) * config.view.scale)
            , y = height / 2 + ((plane.y - config.view.centre.y) * config.view.scale)
            }

        project position =
            toScreen (LatLon.project (Route.projection config.route) position)

        allPoints =
            NonEmpty.toList (Route.points config.route)
    in
    Svg.svg
        (SvgAttr.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
            :: attributes
        )
        (List.concat
            [ [ line (List.map (.plane >> toScreen) allPoints) Theme.routeColour "2.6" ]
            , [ line
                    (List.filter (\point -> Km.isAtOrBefore config.reached point.km) allPoints
                        |> List.map (.plane >> toScreen)
                    )
                    Theme.passedColour
                    "2.6"
              ]
            , List.map (checkpointDot config project) config.checkpoints
            , List.map (breadcrumb project) config.breadcrumbs
            , backToRoute config project
            , youAreHere config project
            , cursorDot config toScreen
            , [ northArrow ]
            ]
        )


line : List { x : Float, y : Float } -> String -> String -> Svg msg
line screenPoints colour thickness =
    Svg.path
        [ SvgAttr.d
            (screenPoints
                |> List.indexedMap
                    (\index point ->
                        (if index == 0 then
                            "M"

                         else
                            "L"
                        )
                            ++ round1 point.x
                            ++ " "
                            ++ round1 point.y
                    )
                |> String.join " "
            )
        , SvgAttr.fill "none"
        , SvgAttr.stroke colour
        , SvgAttr.strokeWidth thickness
        , SvgAttr.strokeLinejoin "round"
        , SvgAttr.strokeLinecap "round"
        ]
        []


checkpointDot : Config -> (LatLon -> { x : Float, y : Float }) -> Checkpoint -> Svg msg
checkpointDot config project checkpoint =
    let
        point =
            Route.atKm config.route checkpoint.km

        screen =
            project point.position

        colour =
            if Checkpoint.isPassed checkpoint || Km.isAtOrBefore config.reached checkpoint.km then
                Theme.passedColour

            else
                Theme.checkpointColour
    in
    Svg.g []
        (Svg.circle
            [ SvgAttr.cx (round1 screen.x)
            , SvgAttr.cy (round1 screen.y)
            , SvgAttr.r "5.5"
            , SvgAttr.fill colour
            , SvgAttr.stroke "#101823"
            , SvgAttr.strokeWidth "1.5"
            ]
            []
            :: (if config.view.scale > 60 then
                    [ Svg.text_
                        [ SvgAttr.x (round1 (screen.x + 9))
                        , SvgAttr.y (round1 (screen.y + 4))
                        , SvgAttr.fill colour
                        , SvgAttr.fontSize "9"
                        , SvgAttr.fontFamily "ui-monospace,monospace"
                        ]
                        [ Svg.text (Checkpoint.displayName checkpoint) ]
                    ]

                else
                    []
               )
        )


breadcrumb : (LatLon -> { x : Float, y : Float }) -> LatLon -> Svg msg
breadcrumb project position =
    let
        screen =
            project position
    in
    Svg.circle
        [ SvgAttr.cx (round1 screen.x)
        , SvgAttr.cy (round1 screen.y)
        , SvgAttr.r "1.6"
        , SvgAttr.fill Theme.passedColour
        ]
        []


{-| The dashed line home. Only drawn when the app is confident the runner has
actually left the course, never on an uncertain reading.
-}
backToRoute : Config -> (LatLon -> { x : Float, y : Float }) -> List (Svg msg)
backToRoute config project =
    case ( config.lost, config.you, config.snapTo ) of
        ( True, Measured fix, Just target ) ->
            let
                from =
                    project fix.at

                to =
                    project target
            in
            [ Svg.line
                [ SvgAttr.x1 (round1 from.x)
                , SvgAttr.y1 (round1 from.y)
                , SvgAttr.x2 (round1 to.x)
                , SvgAttr.y2 (round1 to.y)
                , SvgAttr.stroke Theme.lostColour
                , SvgAttr.strokeWidth "2"
                , SvgAttr.strokeDasharray "5 4"
                ]
                []
            ]

        _ ->
            []


{-| The runner. A measured position is ringed by the accuracy the fix reported,
which is the honest way to say how much the dot can be trusted; a declared one
claims no accuracy and so wears no ring.
-}
youAreHere : Config -> (LatLon -> { x : Float, y : Float }) -> List (Svg msg)
youAreHere config project =
    case config.you of
        Declared ->
            [ dot (project (Route.atKm config.route config.reached).position) Theme.youColour ]

        Measured fix ->
            let
                screen =
                    project fix.at

                colour =
                    if config.lost then
                        Theme.lostColour

                    else if config.uncertain then
                        Theme.uncertainColour

                    else
                        Theme.youColour

                ringRadius =
                    Basics.max 3 (Distance.inKilometers fix.accuracy * config.view.scale)
            in
            [ Svg.circle
                [ SvgAttr.cx (round1 screen.x)
                , SvgAttr.cy (round1 screen.y)
                , SvgAttr.r (round1 ringRadius)
                , SvgAttr.fill colour
                , SvgAttr.opacity ".10"
                , SvgAttr.stroke colour
                , SvgAttr.strokeOpacity ".35"
                ]
                []
            , dot screen colour
            ]


dot : { x : Float, y : Float } -> String -> Svg msg
dot screen colour =
    Svg.circle
        [ SvgAttr.cx (round1 screen.x)
        , SvgAttr.cy (round1 screen.y)
        , SvgAttr.r "5"
        , SvgAttr.fill colour
        , SvgAttr.stroke "#101823"
        , SvgAttr.strokeWidth "1.5"
        ]
        []


{-| Mirrors the elevation cursor, so dragging the chart shows where that point
sits on the ground.
-}
cursorDot : Config -> (Plane -> { x : Float, y : Float }) -> List (Svg msg)
cursorDot config toScreen =
    case config.cursor of
        Nothing ->
            []

        Just km ->
            let
                point =
                    Route.atKm config.route km

                screen =
                    toScreen point.plane
            in
            [ Svg.circle
                [ SvgAttr.cx (round1 screen.x)
                , SvgAttr.cy (round1 screen.y)
                , SvgAttr.r "5.5"
                , SvgAttr.fill Theme.scrubColour
                , SvgAttr.stroke "#101823"
                , SvgAttr.strokeWidth "1.5"
                ]
                []
            , Svg.text_
                [ SvgAttr.x (round1 screen.x)
                , SvgAttr.y (round1 (screen.y - 12))
                , SvgAttr.fill Theme.scrubColour
                , SvgAttr.fontSize "10"
                , SvgAttr.textAnchor "middle"
                , SvgAttr.fontFamily "ui-monospace,monospace"
                ]
                [ Svg.text ("km " ++ Km.toString point.km) ]
            ]


northArrow : Svg msg
northArrow =
    Svg.g [ SvgAttr.opacity ".8" ]
        [ Svg.line
            [ SvgAttr.x1 (String.fromFloat (width - 22))
            , SvgAttr.y1 "30"
            , SvgAttr.x2 (String.fromFloat (width - 22))
            , SvgAttr.y2 "12"
            , SvgAttr.stroke "#8DA1B4"
            , SvgAttr.strokeWidth "1.4"
            ]
            []
        , Svg.text_
            [ SvgAttr.x (String.fromFloat (width - 22))
            , SvgAttr.y "42"
            , SvgAttr.fill "#8DA1B4"
            , SvgAttr.fontSize "9"
            , SvgAttr.textAnchor "middle"
            , SvgAttr.fontFamily "ui-monospace,monospace"
            ]
            [ Svg.text "B" ]
        ]


{-| How many SVG units one CSS pixel covers, given the width the SVG is
rendered at. The gesture maths works in SVG units; the browser reports
pixels.
-}
unitsPerPixel : Float -> Float
unitsPerPixel renderedWidth =
    if renderedWidth > 0 then
        width / renderedWidth

    else
        1


{-| User facing.
-}
gestureHint : String
gestureHint =
    "Kéo để đi, chụm 2 ngón để phóng"


round1 : Float -> String
round1 value =
    String.fromFloat (Basics.toFloat (round (value * 10)) / 10)
