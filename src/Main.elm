module Main exposing (Model(..), Msg(..), main, rgb, update, view)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html, div, pre, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onMouseEnter)
import Time exposing (Posix)
import Html exposing (b)


xRange = 50
yRange = 25

type CharElem
    = CharElem
        { x : Int
        , y : Int
        , value : Int
        }


charElem : Int -> Int -> Int -> CharElem
charElem x y v =
    CharElem { x = x, y = y, value = v }




type Model
    = Model (Array (Array Int))


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
      xss =
        List.range 0 yRange 
          |> List.map (\y -> 
            List.range 0 xRange |> List.map (\x -> 
            charAlem x y ((x + y) modBy 255)))
        |> List.map Array.fromList |> Array.fromList             
    in
    ( Model xss, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame OnAnimationFrame


type Msg
    = OnAnimationFrame Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noCmd m =
            ( m, Cmd.none )
    in
    case msg of
        OnAnimationFrame _ ->
            case model of
                Model xss ->
                    xss |> Array.map (Array.map updateCell) |> (\m -> ( Model m, Cmd.none ))


updateCell : Int -> Int
updateCell i =
    let
        next =
            i + 1
    in
    if next > 255 then
        0

    else
        next


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
    span [ gray (x + (y * 255)) ] [ text "*" ]


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


oneCharOfTint : Int -> Int -> Html Msg
oneCharOfTint index tint =
    span [ gray tint ] [ text "*" ]


appendBr : Array (Html Msg) -> Array (Html Msg)
appendBr html =
    Array.append (Array.fromList [ Html.br [] [] ]) html


view : Model -> Html Msg
view (Model xss) =
    let
        stars : Array (Html Msg)
        stars =
            xss
                |> Array.map
                    (\xs ->
                        xs |> Array.map oneCharOfTint |> appendBr
                    )
                |> Array.foldr Array.append Array.empty
    in
    div [ Attr.style "font-family" "monospace" ] (stars |> Array.toList)
