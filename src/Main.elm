port module Main exposing (Model, Msg(..), Position, WhichKey(..), isPrime, main)

import Audio exposing (Audio, AudioCmd, AudioData)
import Browser
import Browser.Events
import Html exposing (Html)
import Json.Decode as Decode
import List exposing (length)
import Random
import Svg exposing (circle, ellipse, image, svg, text, text_)
import Svg.Attributes exposing (cx, cy, fill, fontSize, height, r, rx, ry, viewBox, width, x, xlinkHref, y)
import Svg.Events exposing (onClick)
import Time
import Json.Decode
import Json.Encode
import Task
import Time

port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg

main =
    Audio.elementWithAudio { init = init, update = update, view = view, subscriptions = subscriptions ,
         audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }, audio=audio

                           }

audio : AudioData -> Model -> Audio
audio _ model =
        case (model.sound, model.time) of
           (Just a_sound, Just a_time) -> Audio.audio a_sound a_time
           _ -> Audio.silence

-- CONSTANTS


type alias Size =
    { width : Int
    , height : Int
    }
-- Sunrise over the australian outback world exhibit
-- https://www.youtube.com/watch?v=h8dv8ykprf8

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


screenLeft : Int
screenLeft =
    130


screenRight : Int
screenRight =
    680



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
    , sound : Maybe Audio.Source
    , time: Maybe Time.Posix
    }


initGame : ( Model, Cmd Msg , AudioCmd Msg)
initGame =
    ( { gameTicks = 0
      , leaves = []
      , score = 0
        , sound = Nothing
      , koala = Position (gridSize.width * cellSize.width // 2) (gridSize.height * cellSize.height - 50) 0
      , time = Nothing
      }
    , Task.perform HereComesAudioTime Time.now
    , Audio.loadAudio
            SoundLoaded
            "https://interactive-examples.mdn.mozilla.net/media/cc0-audio/t-rex-roar.mp3"
    )


init : () -> ( Model, Cmd Msg ,AudioCmd Msg)
init _ =
    initGame



-- UPDATE


type Msg
    = Tick
    | Key WhichKey
    | PlaceLeaf Int
    | SoundLoaded (Result Audio.LoadError Audio.Source)
    | HereComesAudioTime Time.Posix


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


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg ,AudioCmd msg)
update _ msg model =
    case msg of
        HereComesAudioTime a_time -> ( { model | time = Just a_time }
                    , Cmd.none
                    , Audio.cmdNone
                    )
        SoundLoaded result ->
            case result of
                Ok sound ->
                    ( { model | sound = Just sound }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                Err err ->
                    let _ = Debug.log "error" err in
                        ( model
                        , Cmd.none
                        , Audio.cmdNone
                        )


        Tick ->
            let
                --foo = Debug.log "foo" model
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
                , Audio.cmdNone
            )

        PlaceLeaf pos ->
            ( { model | leaves = Position pos 20 0 :: model.leaves }, Cmd.none , Audio.cmdNone)

        Key whichKey ->
            let
                koala =
                    model.koala

                newKoala =
                    { koala | x = onScreen (koala.x + getShift whichKey) }
            in
            ( { model | koala = newKoala }, Cmd.none ,Audio.cmdNone)


onScreen : Int -> Int
onScreen x =
    min (max x screenLeft) screenRight


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
    Random.generate PlaceLeaf (Random.int screenLeft screenRight)



-- SUBSCRIPTIONS


subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Time.every tickFrequency (\_ -> Tick)
        ]



-- VIEW


str : Int -> String
str =
    String.fromInt


view : AudioData -> Model -> Html Msg
view _ model =
    svg
        [ width "100%"
        , height "auto"
        , viewBox ("0 0 " ++ String.fromInt (gridSize.width * cellSize.width) ++ " " ++ String.fromInt (gridSize.height * cellSize.height))
        , Svg.Attributes.style "touch-action: none"
        ]
        (image [ x (String.fromInt 0), y (String.fromInt 0), width (String.fromInt (gridSize.width * cellSize.width)), height (String.fromInt (gridSize.height * cellSize.height)), xlinkHref "assets/background.png" ] []
            :: List.map renderLeaf model.leaves
            ++ [ text_ [ x "120", y "20", Svg.Attributes.style "fill: white" ] [ text ("Score: " ++ String.fromInt model.score) ], text_ [ x "260", y "60", fontSize "96", Svg.Attributes.style "fill: white", onClick (Key LeftArrow) ] [ text "←" ], text_ [ x "520", y "60", fontSize "96", Svg.Attributes.style "fill: white", onClick (Key RightArrow) ] [ text "→" ] ]
            ++ [ image [ x (String.fromInt (model.koala.x - 75)), y (String.fromInt (model.koala.y - 80)), width "150px", height "150px", xlinkHref "assets/koala_mouth_closed.png" ] [] ]
            -- A faster way would be to check primality once, instead of on every tick or every render
            ++ (if isPrime model.score then
                    thinkPrime model.koala

                else
                    []
               )
            ++ (if model.score >= 19 then
                    [ text_ [ x "300", y "200", Svg.Attributes.style "fill: white" ] [ text "Happy birthday!" ] ]

                else
                    []
               )
        )


renderLeaf : Position -> Html Msg
renderLeaf pos =
    image [ x (String.fromInt pos.x), y (String.fromInt pos.y), width "50px", height "auto", xlinkHref "assets/2leaves.png" ] []


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
    let
        x_offset =
            -20

        y_offset =
            -30
    in
    [ renderCircle "white" 5 (Position (koala.x - 10 + x_offset) (koala.y + y_offset) 0)
    , renderCircle "white" 5 (Position (koala.x - 20 + x_offset) (koala.y - 10 + y_offset) 0)
    , renderCircle "white" 10 (Position (koala.x - 30 + x_offset) (koala.y - 25 + y_offset) 0)
    , ellipse [ cx (str (koala.x - 60 + x_offset)), cy (str (koala.y - 65 + y_offset)), rx (str 50), ry (str 30), fill "white" ] []
    , text_ [ x (str (koala.x - 100 + x_offset)), y (str (koala.y - 65 + y_offset)), Svg.Attributes.style "fill: black", fontSize "13" ] [ text "That's prime" ]
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
