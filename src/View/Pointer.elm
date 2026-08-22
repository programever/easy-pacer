module View.Pointer exposing (drag, scrub, wheel)

{-| Pointer events as attributes, so a chart or a map can be dragged without
the view knowing anything about the model.

Everything here reads positions relative to the element the handler is on,
which only holds if that element is the event target. The stylesheet turns
pointer events off on the children of both SVGs for exactly this reason.

-}

import Html exposing (Attribute)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import State exposing (Pixel)


{-| Press or drag across an element, reporting the horizontal offset and the
element's width, so the caller can turn the two into a fraction. A move
without the button held is not a drag and produces no message.
-}
scrub : (Float -> Float -> msg) -> List (Attribute msg)
scrub toMsg =
    let
        position =
            Decode.map2 toMsg
                (Decode.field "offsetX" Decode.float)
                (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
    in
    [ Html.Events.custom "pointerdown" (Decode.map consumed position)
    , Html.Events.custom "pointermove" (Decode.map consumed (whilePressed position))
    ]


{-| Multi-touch drag: each finger is reported with its id, so the model can
tell one finger panning from two fingers pinching. `pointerleave` releases a
mouse that was dragged off the element, which would otherwise stay pressed
forever; touches are captured to their target by the browser and do not need
it.
-}
drag :
    { down : Int -> Pixel -> msg
    , move : Int -> Pixel -> Float -> msg
    , up : Int -> msg
    }
    -> List (Attribute msg)
drag handlers =
    let
        pointerId =
            Decode.field "pointerId" Decode.int

        pixel =
            Decode.map2 Pixel
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)

        width =
            Decode.at [ "currentTarget", "clientWidth" ] Decode.float

        up =
            Decode.map handlers.up pointerId
    in
    [ Html.Events.custom "pointerdown" (Decode.map consumed (Decode.map2 handlers.down pointerId pixel))
    , Html.Events.custom "pointermove"
        (Decode.map consumed (Decode.map3 handlers.move pointerId pixel width))
    , Html.Events.on "pointerup" up
    , Html.Events.on "pointercancel" up
    , Html.Events.on "pointerleave" up
    ]


{-| Mouse wheel, reporting the vertical delta. Prevented from scrolling the
page, since a map that scrolls the page out from under the cursor is not a
map anyone can zoom.
-}
wheel : (Float -> msg) -> Attribute msg
wheel toMsg =
    Html.Events.custom "wheel"
        (Decode.map (consumed << toMsg) (Decode.field "deltaY" Decode.float))


{-| Only while the primary button, or a finger, is down.
-}
whilePressed : Decoder a -> Decoder a
whilePressed decoder =
    Decode.field "buttons" Decode.int
        |> Decode.andThen
            (\buttons ->
                if buttons == 1 then
                    decoder

                else
                    Decode.fail "not pressed"
            )


consumed : msg -> { message : msg, stopPropagation : Bool, preventDefault : Bool }
consumed message =
    { message = message, stopPropagation = False, preventDefault = True }
