port module Main exposing (Model, Msg(..), Position, WhichKey(..), isPrime, main)

-- TODO Delete this
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
port playFromElm : String -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg

main =
    Audio.elementWithAudio { init = init, update = update, view = view, subscriptions = subscriptions ,
         audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }, audio=audio

                           }

audio : AudioData -> Model -> Audio
audio _ model = Audio.silence

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

type alias LeafPosition =
    { x : Int
    , y : Int
    , y_vel : Int
    , leafType : LeafType
    }

type alias Position =
    { x : Int
    , y : Int
    }

type Model = ActiveModel AnActiveModel | InactiveModel

type alias AnActiveModel =
    { gameTicks : Int
    , leaves : List LeafPosition
    , score : Int
    , koala : Position
    , sound : Maybe Audio.Source
    , time: Maybe Time.Posix
    , mouthOpen: Bool
    }


initActiveModel : ( Model, Cmd Msg , AudioCmd Msg)
initActiveModel =
    ( ActiveModel { gameTicks = 0
      , leaves = []
      , score = 0
        , sound = Nothing
      , koala = Position (gridSize.width * cellSize.width // 2) (gridSize.height * cellSize.height - 50)
      , time = Nothing
      , mouthOpen = False
      }
    , Cmd.none
    , Audio.cmdNone
    )


init : () -> ( Model, Cmd Msg , AudioCmd Msg)
init _ = (InactiveModel, Cmd.none, Audio.cmdNone)




-- UPDATE


type Msg
    = Tick
    | Key WhichKey
    | PlaceLeaf (Int, LeafType)
    | SoundLoaded (Result Audio.LoadError Audio.Source)
    | HereComesAudioTime Time.Posix
    | StartClick

type LeafType = OneLeaf | TwoLeaves

applyGravity : LeafPosition -> LeafPosition
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


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg ,AudioCmd Msg)
update _ msg model =
    case model of
        InactiveModel ->
            if msg == StartClick then
                initActiveModel
            else
                (model, Cmd.none, Audio.cmdNone)
        ActiveModel act_model ->
            case msg of
                HereComesAudioTime a_time -> ( ActiveModel { act_model | time = Just a_time }
                            , Cmd.none
                            , Audio.cmdNone
                            )
                SoundLoaded result->
                    case result of
                        Ok sound ->
                            ( ActiveModel { act_model | sound = Just sound }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                        Err err ->
                            --let _ = Debug.log "error" err in
                                ( model
                                , Cmd.none
                                , Audio.cmdNone
                                )


                Tick ->
                    let
                        --foo = Debug.log "foo" model
                        movedLeaves =
                            List.map applyGravity act_model.leaves

                        onScreenLeaves =
                            List.filter (\leaf -> leaf.y < gridSize.height * cellSize.height) movedLeaves

                        unseenLeaves =
                            List.filter (outOfSeeingRange act_model.koala) onScreenLeaves

                        nonEatenLeaves =
                            List.filter (outOfEatingRange act_model.koala) onScreenLeaves

                        nextScore =
                            act_model.score + length onScreenLeaves - length nonEatenLeaves

                        nextMouthOpen = (length unseenLeaves /= length nonEatenLeaves)
                        nextModel =
                            { act_model
                                | leaves = nonEatenLeaves
                                , score = nextScore
                                , mouthOpen = nextMouthOpen
                                , gameTicks = act_model.gameTicks + 1
                            }
                    in
                    ( ActiveModel nextModel
                    , Cmd.batch [if modBy 10 act_model.gameTicks == 0 then generateLeaf else Cmd.none, if nextScore == year && act_model.score /= year then playFromElm "assets/happy_birthday.m4a" else Cmd.none, if act_model.score /= nextScore then playFromElm "assets/nom.wav" else Cmd.none]
                        , Audio.cmdNone
                    )

                PlaceLeaf (pos, leafType) ->
                    ( ActiveModel { act_model | leaves = LeafPosition pos 20 0 leafType :: act_model.leaves }, Cmd.none , Audio.cmdNone)

                StartClick ->
                    ( model , Cmd.none , Audio.cmdNone)
                Key whichKey ->
                    let
                        koala =
                            act_model.koala

                        newKoala =
                            { koala | x = onScreen (koala.x + getShift whichKey) }
                    in
                        -- This isn't enough---they have to click in order for it to go
                    ( ActiveModel { act_model | koala = newKoala }, Cmd.none ,Audio.cmdNone)


onScreen : Int -> Int
onScreen x =
    min (max x screenLeft) screenRight

outOfRange : Int -> Position -> LeafPosition -> Bool
outOfRange distance koala leaf =
    abs (koala.x - leaf.x) > distance || abs (koala.y - leaf.y) > distance

outOfEatingRange : Position -> LeafPosition -> Bool
outOfEatingRange = outOfRange 30

outOfSeeingRange : Position -> LeafPosition -> Bool
outOfSeeingRange = outOfRange 100

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
    Random.generate PlaceLeaf (Random.pair (Random.int screenLeft screenRight) (Random.uniform OneLeaf [TwoLeaves]))


-- SUBSCRIPTIONS


subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Time.every tickFrequency (\_ -> Tick) ]



str : Int -> String
str =
    String.fromInt


view : AudioData -> Model -> Html Msg
view _ model =
    case model of
        InactiveModel ->
            svg
                [ width "100%"
                , height "auto"
                , viewBox ("0 0 " ++ String.fromInt (gridSize.width * cellSize.width) ++ " " ++ String.fromInt (gridSize.height * cellSize.height))
                , Svg.Attributes.style "touch-action: none"
                ]
                [image [ x (String.fromInt 0), y (String.fromInt 0), width (String.fromInt (gridSize.width * cellSize.width)), height (String.fromInt (gridSize.height * cellSize.height)), xlinkHref "assets/background.png" ] [] ,text_ [ x "200", y "60", fontSize "32", Svg.Attributes.style "fill: white", onClick (StartClick) ] [ text "Click on 🐨 emoji to start" ]]
        ActiveModel act_model ->
            svg
                [ width "100%"
                , height "auto"
                , viewBox ("0 0 " ++ String.fromInt (gridSize.width * cellSize.width) ++ " " ++ String.fromInt (gridSize.height * cellSize.height))
                , Svg.Attributes.style "touch-action: none"
                ]
                (image [ x (String.fromInt 0), y (String.fromInt 0), width (String.fromInt (gridSize.width * cellSize.width)), height (String.fromInt (gridSize.height * cellSize.height)), xlinkHref "assets/background.png" ] []
                    :: List.map renderLeaf act_model.leaves
                    ++ [ text_ [ x "120", y "20", Svg.Attributes.style "fill: white" ] [ text ("Score: " ++ String.fromInt act_model.score) ], text_ [ x "260", y "60", fontSize "96", Svg.Attributes.style "fill: white", onClick (Key LeftArrow) ] [ text "←" ], text_ [ x "520", y "60", fontSize "96", Svg.Attributes.style "fill: white", onClick (Key RightArrow) ] [ text "→" ] ]
                    ++  displayKoala act_model
                    -- A faster way would be to check primality once, instead of on every tick or every render
                    ++ (if isPrime act_model.score then
                            thinkPrime act_model.koala

                        else
                            []
                    )
                    ++ (if act_model.score >= year then
                            [ text_ [ x "300", y "200", Svg.Attributes.style "fill: white" ] [ text "Happy birthday!" ], text_ [ x "500", y "300", fontSize "10", Svg.Attributes.style "fill: white" ] [ text "Credits go here" ]]

                        else
                            []
                    )
                )
year = 19

renderLeaf : LeafPosition -> Html Msg
renderLeaf pos =
    image [ x (String.fromInt pos.x), y (String.fromInt pos.y), width "50px", height "auto", xlinkHref (if pos.leafType == OneLeaf then "assets/1leaf.png" else "assets/2leaves.png") ] []


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
    [ renderCircle "white" 5 (Position (koala.x - 10 + x_offset) (koala.y + y_offset))
    , renderCircle "white" 5 (Position (koala.x - 20 + x_offset) (koala.y - 10 + y_offset))
    , renderCircle "white" 10 (Position (koala.x - 30 + x_offset) (koala.y - 25 + y_offset))
    , ellipse [ cx (str (koala.x - 60 + x_offset)), cy (str (koala.y - 65 + y_offset)), rx (str 50), ry (str 30), fill "white" ] []
    , text_ [ x (str (koala.x - 100 + x_offset)), y (str (koala.y - 65 + y_offset)), Svg.Attributes.style "fill: black", fontSize "13" ] [ text "That's prime" ]
    ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)

displayKoala act_model =
    -- Always display both images so that there's no flickering
    -- when loading the second image for the first time
    let displayIt url = image [ x (String.fromInt (act_model.koala.x - 75)), y (String.fromInt (act_model.koala.y - 80)), width "150px", height "150px", xlinkHref url ] [] in
    if act_model.mouthOpen then [displayIt "assets/koala_mouth_closed.png", displayIt "assets/koala_mouth_open.png"] else [displayIt "assets/koala_mouth_open.png", displayIt "assets/koala_mouth_closed.png"]

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
