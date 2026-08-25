module View.Form exposing
    ( button
    , clockField
    , field
    , iconButton
    , miniButton
    , numberField
    , pair
    , pendingButton
    , quietButton
    , solidButton
    , tallButton
    , textField
    , typedNumberField
    )

{-| The small set of controls the app uses.

`clockField` is a plain text input rather than `input type=time`. The native
control refuses to shrink to its container on iOS and spills out of the card,
and when empty it shows nothing at all, so a runner cannot tell what to type.

The typed fields take a blur message as well as an input message. Elm redraws
a controlled input from the model on every keystroke, so a box whose model is
the parsed value rewrites "053" to "00:53" mid-word. The page keeps the raw
text while the box has focus and lets go of it on blur; these controls are
where that second event comes from.

-}

import Html exposing (Attribute, Html, div, input, label, text)
import Html.Attributes as Attr exposing (class, placeholder, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)


button : String -> msg -> Html msg
button content toMsg =
    Html.button [ class "btn", onClick toMsg ] [ text content ]


solidButton : String -> Bool -> msg -> Html msg
solidButton content disabled toMsg =
    Html.button [ class "btn solid", Attr.disabled disabled, onClick toMsg ] [ text content ]


tallButton : String -> msg -> Html msg
tallButton content toMsg =
    Html.button [ class "btn solid tall", onClick toMsg ] [ text content ]


{-| A tall button that shows a second label and refuses clicks while the work
it started is still running.
-}
pendingButton : String -> String -> Bool -> msg -> Html msg
pendingButton content busyContent pending toMsg =
    Html.button
        [ class "btn solid tall", Attr.disabled pending, onClick toMsg ]
        [ text
            (if pending then
                busyContent

             else
                content
            )
        ]


quietButton : String -> msg -> Html msg
quietButton content toMsg =
    Html.button [ class "btn mini quiet", onClick toMsg ] [ text content ]


miniButton : String -> msg -> Html msg
miniButton content toMsg =
    Html.button [ class "btn mini", onClick toMsg ] [ text content ]


iconButton : String -> Bool -> msg -> Html msg
iconButton glyph disabled toMsg =
    Html.button
        [ class "icon-btn", Attr.disabled disabled, onClick toMsg ]
        [ text glyph ]


field : String -> Html msg -> Html msg
field labelText control =
    div [ class "field" ]
        [ label [ class "lbl" ] [ text labelText ], control ]


pair : List (Html msg) -> Html msg
pair children =
    div [ class "pair" ] children


textField : String -> String -> (String -> msg) -> Html msg
textField hintText current toMsg =
    input
        [ type_ "text", placeholder hintText, value current, onInput toMsg ]
        []


numberField : String -> String -> (String -> msg) -> Html msg
numberField hintText current toMsg =
    input (numberAttributes hintText current toMsg) []


typedNumberField : String -> String -> (String -> msg) -> msg -> Html msg
typedNumberField hintText current toMsg onDone =
    input (onBlur onDone :: numberAttributes hintText current toMsg) []


numberAttributes : String -> String -> (String -> msg) -> List (Attribute msg)
numberAttributes hintText current toMsg =
    [ type_ "number"
    , Attr.step "0.1"
    , Attr.min "0"
    , Attr.attribute "inputmode" "decimal"
    , placeholder hintText
    , value current
    , onInput toMsg
    ]


clockField : String -> String -> (String -> msg) -> msg -> Html msg
clockField hintText current toMsg onDone =
    input
        [ type_ "text"
        , class "clock"
        , Attr.attribute "inputmode" "numeric"
        , Attr.maxlength 10
        , placeholder hintText
        , value current
        , onInput toMsg
        , onBlur onDone
        ]
        []
