module Main exposing (Model, Msg(..), Position, WhichKey(..), isPrime, main)

import Browser
import Browser.Events
import Html exposing (Html)
import Json.Decode as Decode
import List exposing (length)
import Random
import Svg exposing (circle, ellipse, image, rect, svg, text, text_)
import Svg.Attributes exposing (cx, cy, fill, fontSize, height, r, rx, ry, viewBox, width, x, xlinkHref, y)
import Svg.Events exposing (onClick)
import Time


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- CONSTANTS


type alias Size =
    { width : Int
    , height : Int
    }


gridSize : Size
gridSize =
    Size 40 20



-- TODO Could combine grid and cell
-- TODO Remove any magic numbers elsewhere


cellSize : Size
cellSize =
    Size 20 20


tickFrequency : Float
tickFrequency =
    100


gravity : Int
gravity =
    1



-- MODEL


type WhichKey
    = LeftArrow
    | RightArrow
    | OtherKey


type alias Position =
    { x : Int
    , y : Int
    , y_vel : Int
    }


type alias Model =
    { gameTicks : Int
    , leaves : List Position
    , score : Int
    , koala : Position
    }


initGame : ( Model, Cmd Msg )
initGame =
    ( { gameTicks = 0
      , leaves = []
      , score = 0
      , koala = Position (gridSize.width * cellSize.width // 2) (gridSize.height * cellSize.height - 50) 0
      }
    , Cmd.none
    )


init : () -> ( Model, Cmd Msg )
init _ =
    initGame



-- UPDATE


type Msg
    = Tick
    | Key WhichKey
    | PlaceLeaf Int


applyGravity : Position -> Position
applyGravity leaf =
    let
        new_y =
            leaf.y + leaf.y_vel

        new_y_vel =
            leaf.y_vel + gravity
    in
    { leaf
        | y = new_y
        , y_vel = new_y_vel
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                movedLeaves =
                    List.map applyGravity model.leaves

                onScreenLeaves =
                    List.filter (\leaf -> leaf.y < gridSize.height * cellSize.height) movedLeaves

                nonEatenLeaves =
                    List.filter (isFar model.koala) onScreenLeaves

                nextScore =
                    model.score + length onScreenLeaves - length nonEatenLeaves

                nextModel =
                    { model
                        | leaves = nonEatenLeaves
                        , score = nextScore
                        , gameTicks = model.gameTicks + 1
                    }
            in
            ( nextModel
            , if modBy 10 model.gameTicks == 0 then
                generateLeaf

              else
                Cmd.none
            )

        PlaceLeaf pos ->
            ( { model | leaves = Position pos 20 0 :: model.leaves }, Cmd.none )

        Key whichKey ->
            let
                koala =
                    model.koala

                newKoala =
                    { koala | x = koala.x + getShift whichKey }
            in
            ( { model | koala = newKoala }, Cmd.none )


isFar : Position -> Position -> Bool
isFar koala leaf =
    let
        distance =
            30
    in
    abs (koala.x - leaf.x) > distance || abs (koala.y - leaf.y) > distance


getShift : WhichKey -> Int
getShift key =
    let
        scale =
            20
    in
    case key of
        LeftArrow ->
            -scale

        RightArrow ->
            scale

        OtherKey ->
            0


generateLeaf : Cmd Msg
generateLeaf =
    Random.generate PlaceLeaf (Random.int 0 (cellSize.width * gridSize.width))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Time.every tickFrequency (\_ -> Tick)
        ]



-- VIEW


str : Int -> String
str =
    String.fromInt


view : Model -> Html Msg
view model =
    svg
        [ width "100%"
        , height "auto"
        , viewBox ("0 0 " ++ String.fromInt (gridSize.width * cellSize.width) ++ " " ++ String.fromInt (gridSize.height * cellSize.height))
        , Svg.Attributes.style "touch-action: none"
        ]
        (rect [ width (String.fromInt (gridSize.width * cellSize.width)), height (String.fromInt (gridSize.height * cellSize.height)) ] []
            :: List.map (renderCircle "green" 10) model.leaves
            ++ [ image [ x (String.fromInt (model.koala.x - 25)), y (String.fromInt model.koala.y), width "50px", height "50px", xlinkHref "https://upload.wikimedia.org/wikipedia/commons/4/49/Koala_climbing_tree.jpg" ] [] ]
            -- A faster way would be to check primality once, instead of on every tick or every render
            ++ (if isPrime model.score then
                    thinkPrime model.koala

                else
                    []
               )
            ++ [ text_ [ x "5", y "20", Svg.Attributes.style "fill: white" ] [ text ("Score: " ++ String.fromInt model.score) ], text_ [ x "260", y "60", fontSize "96", Svg.Attributes.style "fill: white", onClick (Key LeftArrow) ] [ text "←" ], text_ [ x "520", y "60", fontSize "96", Svg.Attributes.style "fill: white", onClick (Key RightArrow) ] [ text "→" ] ]
            ++ (if model.score >= 19 then
                    [ text_ [ x "300", y "200", Svg.Attributes.style "fill: white" ] [ text "Happy birthday!" ] ]

                else
                    []
               )
        )


renderCircle : String -> Int -> Position -> Html Msg
renderCircle color radius pos =
    circle
        [ cx (String.fromInt pos.x)
        , cy (String.fromInt pos.y)
        , r (String.fromInt radius)
        , fill color
        ]
        []



-- Adapted from https://github.com/MartinSnyder/elm-snake
-- TODO Add credits in readme


thinkPrime : Position -> List (Html Msg)
thinkPrime koala =
    [ renderCircle "white" 5 (Position (koala.x - 10) koala.y 0)
    , renderCircle "white" 5 (Position (koala.x - 20) (koala.y - 10) 0)
    , renderCircle "white" 10 (Position (koala.x - 30) (koala.y - 25) 0)
    , ellipse [ cx (str (koala.x - 60)), cy (str (koala.y - 65)), rx (str 50), ry (str 30), fill "white" ] []
    , text_ [ x (str (koala.x - 100)), y (str (koala.y - 65)), Svg.Attributes.style "fill: black", fontSize "13" ] [ text "That's prime" ]
    ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


isPrime : Int -> Bool
isPrime n =
    if n < 2 then
        False

    else
        List.range 2 (n - 1) |> List.all (\x -> modBy x n /= 0)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            Key LeftArrow

        "ArrowRight" ->
            Key RightArrow

        _ ->
            Key OtherKey
