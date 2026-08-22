module View.Theme exposing
    ( card
    , checkpointColour
    , eyebrow
    , hint
    , ledgerRow
    , lostColour
    , note
    , passedColour
    , quadCell
    , routeColour
    , routeStateClass
    , scrubColour
    , sectionTitle
    , uncertainColour
    , urgencyClass
    , verdict
    , youColour
    )

{-| Class names and palette, in one place.

The stylesheet lives in `public/style.css` and is shared with the previous
build, so the design survives the port unchanged. Elm only names classes; it
never inlines styles.

One rule the palette encodes: every colour means exactly one thing. Checkpoints
were yellow and so was the elevation cursor, which made the two impossible to
tell apart while dragging a finger across the chart.

-}

import Core.App.Position exposing (RouteState(..))
import Core.App.Segment exposing (Urgency(..))
import Html exposing (Attribute, Html, div, p, span, text)
import Html.Attributes exposing (class)


routeColour : String
routeColour =
    "#FF7A2F"


passedColour : String
passedColour =
    "#5A6B7D"


checkpointColour : String
checkpointColour =
    "#6EC1FF"


youColour : String
youColour =
    "#35D0A5"


uncertainColour : String
uncertainColour =
    "#FFC53D"


lostColour : String
lostColour =
    "#FF5C6C"


{-| The elevation cursor. Shares the warning hue on purpose: it is the only
other thing on screen the runner is actively pointing at.
-}
scrubColour : String
scrubColour =
    "#FFC53D"


eyebrow : String -> Html msg
eyebrow content =
    div [ class "lead-eyebrow" ] [ text content ]


sectionTitle : String -> Html msg
sectionTitle content =
    Html.h2 [] [ text content ]


note : String -> Html msg
note content =
    p [ class "note" ] [ text content ]


hint : String -> Html msg
hint content =
    div [ class "hint" ] [ text content ]


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attributes children =
    div (class "lead" :: attributes) children


quadCell : String -> String -> String -> Html msg
quadCell label value unit =
    div []
        [ div [ class "mini-k" ] [ text label ]
        , div [ class "mini-v" ]
            (text value
                :: (if unit == "" then
                        []

                    else
                        [ Html.small [] [ text unit ] ]
                   )
            )
        ]


ledgerRow : List (Attribute msg) -> List (Html msg) -> Html msg
ledgerRow attributes children =
    div (class "lrow" :: attributes) children


verdict : Urgency -> String -> Maybe String -> Html msg
verdict level headline detail =
    div [ class ("verdict " ++ urgencyClass level) ]
        (text headline
            :: (case detail of
                    Nothing ->
                        []

                    Just extra ->
                        [ span [] [ text extra ] ]
               )
        )


urgencyClass : Urgency -> String
urgencyClass level =
    case level of
        NoDeadline ->
            "none"

        Comfortable ->
            "ok"

        Tight ->
            "warn"

        Missed ->
            "late"


routeStateClass : RouteState -> String
routeStateClass state =
    case state of
        OnRoute ->
            "guide on-route"

        Uncertain ->
            "guide unsure"

        OffRoute ->
            "guide off-route"
