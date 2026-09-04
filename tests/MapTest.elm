module MapTest exposing (suite)

{-| The arrows that say which way the course runs. Stated on plain screen
lines, with the browser out of the picture.
-}

import Expect
import Test exposing (Test, describe, test)
import View.Map as Map


suite : Test
suite =
    describe "Direction arrows along the course"
        [ test "a line running right gets arrows pointing right" <|
            \_ ->
                Map.arrowsAlong [ { x = 10, y = 50 }, { x = 210, y = 50 } ]
                    |> List.map .angle
                    |> List.all (\angle -> abs angle < 0.001)
                    |> Expect.equal True
        , test "a line running down the screen gets arrows pointing down" <|
            \_ ->
                Map.arrowsAlong [ { x = 50, y = 10 }, { x = 50, y = 210 } ]
                    |> List.map .angle
                    |> List.all (\angle -> abs (angle - 90) < 0.001)
                    |> Expect.equal True
        , test "arrows are spaced evenly along the screen line" <|
            \_ ->
                let
                    xs =
                        Map.arrowsAlong [ { x = 0, y = 50 }, { x = 200, y = 50 } ]
                            |> List.map .x

                    gaps =
                        List.map2 (-) (List.drop 1 xs) xs
                in
                case gaps of
                    first :: rest ->
                        List.all (\gap -> abs (gap - first) < 0.001) rest
                            |> Expect.equal True

                    [] ->
                        Expect.fail "a 200 unit line should carry more than one arrow"
        , test "spacing survives a corner between two segments" <|
            \_ ->
                let
                    straight =
                        Map.arrowsAlong [ { x = 0, y = 50 }, { x = 200, y = 50 } ]

                    bent =
                        Map.arrowsAlong [ { x = 0, y = 50 }, { x = 100, y = 50 }, { x = 100, y = 150 } ]
                in
                Expect.equal (List.length bent) (List.length straight)
        , test "a line running back the way it came flips its arrows" <|
            \_ ->
                Map.arrowsAlong [ { x = 210, y = 50 }, { x = 10, y = 50 } ]
                    |> List.map .angle
                    |> List.all (\angle -> abs (abs angle - 180) < 0.001)
                    |> Expect.equal True
        , test "arrows off the canvas are not drawn" <|
            \_ ->
                Map.arrowsAlong [ { x = -500, y = 50 }, { x = -300, y = 50 } ]
                    |> Expect.equal []
        , test "a zero length segment places nothing and breaks nothing" <|
            \_ ->
                Map.arrowsAlong [ { x = 50, y = 50 }, { x = 50, y = 50 } ]
                    |> Expect.equal []
        ]
