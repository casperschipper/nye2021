module Main exposing (Model(..), Msg(..), main, update, view, rgb)

import Browser
import Html exposing (Html, div, pre, span, text)
import Html.Attributes as Attr
import Random


type Model
    = Model


main : Program () Model Msg
main =
    Browser.sandbox { init = Model, update = update, view = view }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg _ =
    case msg of
        NoOp ->
            Model


rgb : Int -> Int -> Int -> String
rgb red green blue =
    let
        f =
            String.fromInt
    in
    ( f red, f green, f blue ) |> (\( r, g, b ) -> [ "rgb(", r, ",", g, ",", b, ")" ] |> String.concat)


divideBy : Float -> Float -> Float
divideBy y x =
    x / y


gray : Int -> Html.Attribute Msg
gray tint =
    let
        x =
            tint |> toFloat |> divideBy 255.0 |> floor
    in
    Attr.style "color" (rgb x x x)


charXY : Int -> Int -> Html Msg
charXY x y =
    span [ gray (x + (y * 255))] [ text "*" ]


andThen : (a -> List b) -> List a -> List b
andThen f xs =
    xs |> List.map f |> List.concat


return : a -> List a
return x =
    [ x ]


manyStars : List (Html Msg)
manyStars =
    List.range 0 100
        |> andThen
            (\x ->
                List.range 0 100
                    |> andThen
                        (\y ->
                            return (charXY x y)
                        )
            )


view : Model -> Html Msg
view _ =
    div [ Attr.style "font-family" "monospace" ] manyStars
