module Main exposing (Model(..), Msg(..), main, rgb, update, view)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html, b, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onMouseEnter)
import Time exposing (Posix)


xRange : number
xRange =
    64


yRange : number
yRange =
    32


type CharElem
    = CharElem
        { x : Int
        , y : Int
        , value : Float
        }


charElem : Int -> Int -> Float -> CharElem
charElem x y v =
    CharElem { x = x, y = y, value = v }


type Model
    = Model Int (Array (Array CharElem))


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        xss =
            List.range 0 yRange
                |> List.map
                    (\y ->
                        List.range 0 xRange
                            |> List.map
                                (\x ->
                                    charElem x y 0
                                )
                    )
                |> List.map Array.fromList
                |> Array.fromList
    in
    ( Model 0 xss, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame OnAnimationFrame


type Msg
    = OnAnimationFrame Posix
    | OnMouseEnter Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrame _ ->
            case model of
                Model x xss ->
                    case modBy 1 x of
                        0 ->
                            xss |> Array.map (Array.map updateCell) |> (\m -> ( Model (x + 1) m, Cmd.none ))

                        _ ->
                            ( Model (x + 1) xss, Cmd.none )

        OnMouseEnter x y ->
            ( resetCell x y model, Cmd.none )


updateAtIndex : (a -> a) -> Int -> Array a -> Array a
updateAtIndex f idx array =
    Array.get idx array
        |> Maybe.map (\old -> Array.set idx (f old) array)
        |> Maybe.withDefault array


resetCell : Int -> Int -> Model -> Model
resetCell cellX cellY (Model t xss) =
    let
        myF (CharElem { x, y }) =
            CharElem { x = x, y = y, value = 255.0 }
    in
    Model t (updateAtIndex (\ys -> updateAtIndex myF cellX ys) cellY xss)


updateCell : CharElem -> CharElem
updateCell (CharElem { x, y, value }) =
    let
        next =
            value - 0.1
    in
    if next < 0 then
        charElem x y 0

    else
        charElem x y next


rgb : Int -> Int -> Int -> String
rgb red green blue =
    let
        f =
            String.fromInt
    in
    ( f red, f green, f blue ) |> (\( r, g, b ) -> [ "rgb(", r, ",", g, ",", b, ")" ] |> String.concat)




gray : Float -> Html.Attribute Msg
gray x =
    let
        xf =
            x / 255.0

        tau =
            2 * pi

        w =
            xf * tau * 5

        ( r, g, b ) =
            ( sin w, sin (w + (tau / 3.0)), sin (w + (tau / 3.0)) )

        int255 i =
            i * 255.0 |> floor
    in
    Attr.style "color" (rgb (int255 r) (int255 g) (int255 b))

{-
andThen : (a -> List b) -> List a -> List b
andThen f xs =
    xs |> List.map f |> List.concat
    


return : a -> List a
return x =
    [ x ]
    
-}

-- viewChar2 : CharElem -> Html Msg
-- viewChar2 (CharElem { x, y, value }) =
--     let
--         xstr =
--             String.fromInt x

--         ystr =
--             String.fromInt y

--         vstr =
--             String.fromFloat value
--     in
--     span [ gray value, onMouseEnter (OnMouseEnter x y) ] [ text (String.join " " [ "  -", xstr, ystr, vstr, "-  " ]) ]


viewChar : CharElem -> Html Msg
viewChar (CharElem { x, y, value }) =
    span [ gray value, onMouseEnter (OnMouseEnter x y), Attr.style "width" "0.7em", Attr.style "display" "inline-block"  ] [ text (charOfValue value) ]


charOfValue : Float -> String
charOfValue flt =
    let
        int =
            floor flt
    in
    Char.fromCode int |> String.fromChar
    -- case int // 32 of
    --     0 ->
    --         "`"

    --     1 ->
    --         "\\"

    --     2 ->
    --         "/"

    --     3 ->
    --         ","

    --     4 ->
    --         "-"

    --     5 ->
    --         ","

    --     6 ->
    --         ","

    --     7 ->
    --         "."

    --     _ ->
    --         "*"


appendBr : Array (Html Msg) -> Array (Html Msg)
appendBr html =
    Array.append (Array.fromList [ Html.br [] [] ]) html


view : Model -> Html Msg
view (Model t xss) =
    let
        stars : Array (Html Msg)
        stars =
            xss
                |> Array.map
                    (\xs ->
                        xs |> Array.map viewChar |> appendBr
                    )
                |> Array.foldr Array.append Array.empty
    in
    div []
        [ Html.p [] [ text (String.fromInt t) ]
        , div
            [ Attr.style "font-family" "monospace"
            , Attr.style "font-size" "24px"
            , Attr.style "overflow" "hide"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            ]
            (stars |> Array.toList)
        ]
