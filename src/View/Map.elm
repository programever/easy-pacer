module View.Map exposing (Arrow, Config, You(..), arrowsAlong, unitsPerPixel, view)

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
import Core.Data.Distance as Distance exposing (Distance)
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

    -- How far past `reached` the course carries direction arrows.
    , ahead : Distance
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


{-| Screen distance between two direction arrows, in SVG units. Spaced on the
screen rather than along the course, so zooming in reveals more arrows instead
of stretching the same few apart.
-}
arrowSpacing : Float
arrowSpacing =
    44


view : List (Attribute msg) -> Config -> Svg msg
view attributes config =
    let
        toScreen plane =
            { x = width / 2 + ((plane.x - config.view.centre.x) * config.view.scale)
            , y = height / 2 + ((plane.y - config.view.centre.y) * config.view.scale)
            }

        project position =
            toScreen (LatLon.project (Route.projection config.route) position)

        onScreen =
            List.map (.plane >> toScreen)

        -- Behind the runner, then ahead on top, both cut exactly at the
        -- runner's km. Where the course uses the same ground twice the part
        -- still to run must win, so it is drawn last; the grey underneath is
        -- wider than the orange, so shared ground shows orange with a grey
        -- edge, and a glance still says it has been run once already.
        behind =
            Route.slice config.route Km.start config.reached

        ahead =
            Route.slice config.route config.reached (Route.totalKm config.route)
    in
    Svg.svg
        (SvgAttr.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
            :: attributes
        )
        (List.concat
            [ [ line (onScreen behind) Theme.passedColour "5" ]
            , [ line (onScreen ahead) Theme.routeColour "2.6" ]
            , List.map directionArrow (arrowsAlong (onScreen (stretchAhead config)))
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


{-| A point on the screen and the direction the course runs through it, in
degrees clockwise from screen-right.
-}
type alias Arrow =
    { x : Float, y : Float, angle : Float }


{-| The piece of course that carries direction arrows: from the runner forward
for `ahead`. Arrows on the whole course would point both ways at once wherever
an out-and-back passes itself, which is exactly where the runner needs a
single answer.
-}
stretchAhead : Config -> List Route.Point
stretchAhead config =
    Route.slice config.route config.reached (Km.advance config.ahead config.reached)


{-| Where to draw the arrows that say which way the course is run. On a loop or
an out-and-back the line alone does not say, and the place the runner most
needs to know is exactly the junction where two passes meet.

Walks the drawn line in screen units, dropping an arrow every `arrowSpacing`,
each turned to follow its own segment. Arrows off the visible canvas are left
out, so a zoomed-in map only pays for what it shows.

-}
arrowsAlong : List { x : Float, y : Float } -> List Arrow
arrowsAlong screenPoints =
    List.map2 Tuple.pair screenPoints (List.drop 1 screenPoints)
        |> List.foldl placeAlong ( arrowSpacing / 2, [] )
        |> Tuple.second
        |> List.filter onCanvas
        |> List.reverse


{-| Newest-first; `arrowsAlong` reverses at the end. `untilNext` is how much
of the screen distance to the next arrow is still to be walked.
-}
placeAlong : ( { x : Float, y : Float }, { x : Float, y : Float } ) -> ( Float, List Arrow ) -> ( Float, List Arrow )
placeAlong ( from, to ) ( untilNext, acc ) =
    let
        dx =
            to.x - from.x

        dy =
            to.y - from.y

        segment =
            sqrt ((dx * dx) + (dy * dy))
    in
    if segment <= 0 then
        ( untilNext, acc )

    else if untilNext > segment then
        ( untilNext - segment, acc )

    else
        let
            fraction =
                untilNext / segment

            arrow =
                { x = from.x + (dx * fraction)
                , y = from.y + (dy * fraction)
                , angle = atan2 dy dx * 180 / pi
                }
        in
        placeAlong ( { x = arrow.x, y = arrow.y }, to ) ( arrowSpacing, arrow :: acc )


onCanvas : Arrow -> Bool
onCanvas arrow =
    arrow.x >= 0 && arrow.x <= width && arrow.y >= 0 && arrow.y <= height


{-| A chevron in the paper colour, sitting on the route line and pointing along
it. Its arms reach past the line onto the dark ground, so the V shape reads at
a glance whether the line under it is the orange course or the grey passed part.
-}
directionArrow : Arrow -> Svg msg
directionArrow arrow =
    Svg.path
        [ SvgAttr.d "M-3.5 -3.5 L1.5 0 L-3.5 3.5"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "#EEF3F7"
        , SvgAttr.strokeWidth "1.8"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        , SvgAttr.transform
            ("translate(" ++ round1 arrow.x ++ " " ++ round1 arrow.y ++ ") rotate(" ++ round1 arrow.angle ++ ")")
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


round1 : Float -> String
round1 value =
    String.fromFloat (Basics.toFloat (round (value * 10)) / 10)
