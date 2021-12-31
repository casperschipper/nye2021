module Main exposing (Model(..), Msg(..), main, message, rgb, update, view)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html, b, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onMouseEnter)
import Time exposing (Posix)


message : Int -> String
message index =
    let
        string =
            "🎄.Best.Wishes.For.2022!"

        safeIndex =
            modBy (String.length string) index
    in
    string |> String.toList |> Array.fromList |> Array.get safeIndex |> Maybe.withDefault '*' |> String.fromChar




type CharElem
    = CharElem
        { x : Int
        , y : Int
        , value : Float
        , messageChar : String
        }


charElem : Int -> Int -> Float -> String -> CharElem
charElem x y v m =
    CharElem { x = x, y = y, value = v, messageChar = m }


type alias Dimensions = (Int,Int)

type Model
    = Model Dimensions Int (Array (Array CharElem))


main : Program (Int,Int) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : ( Int, Int ) -> ( Model, Cmd Msg )
init ( w, h ) =
    let
        (wn,hn) =( w // 33, h // 24)

        xss =
            List.range 0 hn
                |> List.map
                    (\y ->
                        List.range 0 wn
                            |> List.map
                                (\x ->
                                    charElem x y 0 (message (x + (y * wn)))
                                )
                    )
                |> List.map Array.fromList
                |> Array.fromList
    in
    ( Model (wn,hn) 0 xss, Cmd.none )


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
                Model (w,h) x xss ->
                    case modBy 1 x of
                        0 ->
                            xss |> Array.map (Array.map (updateCell (w,h) (xss))) |> (\m -> ( Model (w,h) (x + 1) m, Cmd.none ))

                        _ ->
                            ( Model (w,h) (x + 1) xss, Cmd.none )

        OnMouseEnter x y ->
            ( resetCell x y model, Cmd.none )


updateAtIndex : (a -> a) -> Int -> Array a -> Array a
updateAtIndex f idx array =
    Array.get idx array
        |> Maybe.map (\old -> Array.set idx (f old) array)
        |> Maybe.withDefault array


resetCell : Int -> Int -> Model -> Model
resetCell cellX cellY (Model wh t xss) =
    let
        myF (CharElem { x, y, messageChar }) =
            CharElem { x = x, y = y, value = 2 ^ 20 |> toFloat, messageChar = messageChar }
    in
    Model  wh t (updateAtIndex (\ys -> updateAtIndex myF cellX ys) cellY xss)


getCoordinate : Int -> Int -> Array (Array CharElem) -> CharElem
getCoordinate x y arr =
    Array.get (modBy (Array.length arr) y) arr
        |> Maybe.andThen (\xarr -> Array.get (modBy (Array.length xarr) x) xarr)
        |> Maybe.withDefault (charElem 0 0 0.0 "*")

{-
e : Float
e =
    2.718281828459045


cosh : Float -> Float
cosh x =
    (e ^ x + e ^ -x) / 2.0


sinh : Float -> Float
sinh x =
    (e ^ x - e ^ -x) / 2.0


tanh : Float -> Float
tanh x =
    sinh x / cosh x
-}

calcNeighbours : Int -> Int -> Array (Array CharElem) -> Float
calcNeighbours x y arr =
    let
        top =
            getCoordinate x (y - 1) arr

        topL =
            getCoordinate (x - 1) (y - 1) arr

        topR =
            getCoordinate (x + 1) (y - 1) arr

        right =
            getCoordinate (x + 1) y arr

        bottom =
            getCoordinate x (y + 1) arr

        bottomL =
            getCoordinate (x - 1) (y + 1) arr

        bottomR =
            getCoordinate (x + 1) (y + 1) arr

        left =
            getCoordinate (x - 1) y arr

        attenuate =
            0.359
    in
    ([ bottomL, bottomR, topL, topR ] |> List.foldr (\(CharElem { value }) acc -> value + acc) 0.0 |> (\v -> (v / 5.0) * attenuate))
        + ([ top, right, bottom, left ] |> List.foldr (\(CharElem { value }) acc -> value + acc) 0.0 |> (\v -> (v / 2.0) * attenuate))


updateCell : (Int,Int) -> Array (Array CharElem) -> CharElem -> CharElem
updateCell (xRange,yRange) array (CharElem { x, y, value, messageChar }) =
    let
        old =
            charElem x y value messageChar
    in
    case ( x, y ) of
        ( 0, _ ) ->
            old

        ( _, 0 ) ->
            old

        ( xx, yy ) ->
            if (xx == xRange) || (yy == yRange) then
                old

            else
                let
                    next =
                        calcNeighbours x y array - (0.01 * value)
                in
                if (next < 0) || (next > 262144) then
                    charElem x y -1 messageChar

                else
                    charElem x y next messageChar


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
            x / 262144.0

        tau =
            2 * pi

        w =
            xf * tau * 5

        ( r, g, b ) =
            ( sin (w * 1.0), sin ((w * 1.5) + (tau / 3.0)), sin ((w * 3.0) + (tau / 3.0)) )

        int255 i =
            i * 128.0 |> floor |> (+) 128
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


combi : Float -> String
combi x =
    let
        xx =
            x / 128.0
    in
    if xx > 128.0 then
        charOfValue xx

    else
        xx |> floor |> message


viewChar : CharElem -> Html Msg
viewChar (CharElem { x, y, value, messageChar }) =
    let
        ( tint, str ) =
            if value < 32.0 then
                ( 0.0, messageChar )

            else if value == 0.0 then
                ( 0.0, messageChar )

            else
                ( value, combi value )
    in
    span
        [ gray tint
        , onMouseEnter (OnMouseEnter x y)
        , onClick (OnMouseEnter x y)
        , Attr.style "width" "1.4em"
        , Attr.style "display" "inline-block"
        ]
        [ text str ]


charOfValue : Float -> String
charOfValue flt =
    let
        int =
            floor flt + 32 |> modBy 262144
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


dropFirstAndLast : Array a -> Array a
dropFirstAndLast arr =
    let
        n =
            Array.length arr
    in
    if n < 3 then
        arr

    else
        Array.slice 1 -1 arr


view : Model -> Html Msg
view (Model _ _ xss) =
    let
        stars : Array (Html Msg)
        stars =
            xss
                |> Array.map
                    (\xs ->
                        xs |> Array.map viewChar |> dropFirstAndLast |> appendBr
                    )
                |> dropFirstAndLast
                |> Array.foldr Array.append Array.empty
    in
    div
        [ Attr.style "background-color" "black"
        , Attr.style "white-space" "nowrap"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "24px"
        , Attr.style "line-height" "0.9em"
        , Attr.style "overflow-y" "hide"
        , Attr.style "overflow-x" "hide"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        ]
        ((stars |> Array.toList)
            ++ [ Html.p
                    [ Attr.style "color" "white"
                    , Attr.style "display" "block"
                    , Attr.style "height" "600px"
                    ]
                    [ text "hint: touch or move mouse!" ]
               ]
        )
