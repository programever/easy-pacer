module View.Profile exposing (Config, kmAtFraction, readout, view)

{-| The elevation chart: the course seen side on, with checkpoint ticks, the
runner's position, and an optional cursor.

Drawn as a pure function of the route. Dragging a finger across it is handled by
the page, which passes the pointer attributes in and converts a horizontal
fraction into a milestone through `kmAtFraction`; the chart itself knows
nothing about pointers.

-}

import Core.App.Checkpoint as Checkpoint exposing (Checkpoint)
import Core.App.Km as Km exposing (Km)
import Core.App.Route as Route exposing (Route)
import Core.Data.Distance as Distance
import Core.Data.Elevation as Elevation
import Html exposing (Attribute)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import View.Theme as Theme


type alias Config =
    { route : Route
    , checkpoints : List Checkpoint
    , you : Maybe Km
    , cursor : Maybe Km
    }


width : Float
width =
    320


height : Float
height =
    96


padding : Float
padding =
    6


{-| How many points to sample along the course when drawing. More than this is
invisible on a phone and costs redraws while a finger is moving.
-}
samples : Int
samples =
    160


{-| User facing: what the cursor is pointing at, and what is left from there
to the finish.
-}
readout : Route -> Km -> String
readout route km =
    let
        point =
            Route.atKm route km

        remaining =
            Km.difference km (Route.totalKm route)
    in
    "km "
        ++ Km.toString km
        ++ " · cao "
        ++ String.fromInt (Elevation.inWholeMeters point.elevation)
        ++ " m · từ xuất phát leo "
        ++ String.fromInt (Elevation.inWholeMeters point.ascentSoFar)
        ++ " m, xuống "
        ++ String.fromInt (Elevation.inWholeMeters point.descentSoFar)
        ++ " m · còn "
        ++ Km.toString (Km.fromFloat (Distance.inKilometers remaining))
        ++ " km tới đích, leo "
        ++ String.fromInt (Elevation.inWholeMeters (Route.remainingAscent route km))
        ++ " m, xuống "
        ++ String.fromInt (Elevation.inWholeMeters (Route.remainingDescent route km))
        ++ " m"


{-| Where a horizontal fraction of the chart lands on the course.
-}
kmAtFraction : Route -> Float -> Km
kmAtFraction route fraction =
    Km.fromFloat (clamp 0 1 fraction * Km.toFloat (Route.totalKm route))


view : List (Attribute msg) -> Config -> Svg msg
view attributes config =
    let
        ( low, high ) =
            Route.elevationRange config.route

        span =
            Basics.max 1 (high - low)

        total =
            Basics.max 0.001 (Km.toFloat (Route.totalKm config.route))

        toX km =
            Km.toFloat km / total * width

        toY metres =
            height - padding - ((metres - low) / span * (height - padding * 2))

        outline =
            List.range 0 samples
                |> List.map
                    (\step ->
                        let
                            point =
                                Route.atKm config.route
                                    (Km.fromFloat (total * Basics.toFloat step / Basics.toFloat samples))
                        in
                        ( toX point.km, toY (Elevation.inMeters point.elevation) )
                    )

        path =
            outline
                |> List.indexedMap
                    (\index ( x, y ) ->
                        (if index == 0 then
                            "M"

                         else
                            "L"
                        )
                            ++ round1 x
                            ++ " "
                            ++ round1 y
                    )
                |> String.join " "
    in
    Svg.svg
        (SvgAttr.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
            :: SvgAttr.preserveAspectRatio "none"
            :: attributes
        )
        (List.concat
            [ [ Svg.path
                    [ SvgAttr.d (path ++ " L" ++ String.fromFloat width ++ " " ++ String.fromFloat height ++ " L0 " ++ String.fromFloat height ++ " Z")
                    , SvgAttr.fill "rgba(255,122,47,.16)"
                    ]
                    []
              , Svg.path
                    [ SvgAttr.d path
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke Theme.routeColour
                    , SvgAttr.strokeWidth "1.6"
                    ]
                    []
              ]
            , List.map (checkpointTick toX) (List.filter isTickable config.checkpoints)
            , marker toX toY config.route "#EEF3F7" 4.5 config.you
            , marker toX toY config.route Theme.scrubColour 5 config.cursor
            ]
        )


isTickable : Checkpoint -> Bool
isTickable checkpoint =
    checkpoint.role /= Checkpoint.StartLine


checkpointTick : (Km -> Float) -> Checkpoint -> Svg msg
checkpointTick toX checkpoint =
    Svg.line
        [ SvgAttr.x1 (round1 (toX checkpoint.km))
        , SvgAttr.y1 "0"
        , SvgAttr.x2 (round1 (toX checkpoint.km))
        , SvgAttr.y2 (String.fromFloat height)
        , SvgAttr.stroke Theme.checkpointColour
        , SvgAttr.strokeWidth "1"
        , SvgAttr.strokeDasharray "3 3"
        , SvgAttr.opacity ".45"
        ]
        []


marker : (Km -> Float) -> (Float -> Float) -> Route -> String -> Float -> Maybe Km -> List (Svg msg)
marker toX toY route colour radius maybeKm =
    case maybeKm of
        Nothing ->
            []

        Just km ->
            let
                point =
                    Route.atKm route km

                x =
                    toX point.km

                y =
                    toY (Elevation.inMeters point.elevation)
            in
            [ Svg.line
                [ SvgAttr.x1 (round1 x)
                , SvgAttr.y1 "0"
                , SvgAttr.x2 (round1 x)
                , SvgAttr.y2 (String.fromFloat height)
                , SvgAttr.stroke colour
                , SvgAttr.strokeWidth "1.3"
                ]
                []
            , Svg.circle
                [ SvgAttr.cx (round1 x)
                , SvgAttr.cy (round1 y)
                , SvgAttr.r (String.fromFloat radius)
                , SvgAttr.fill colour
                , SvgAttr.stroke "#101823"
                , SvgAttr.strokeWidth "1.5"
                ]
                []
            ]


round1 : Float -> String
round1 value =
    String.fromFloat (Basics.toFloat (round (value * 10)) / 10)
