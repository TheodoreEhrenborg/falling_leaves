port module Main exposing (LeafType, Model, Msg(..), Position, WhichKey(..), isPrime, main)

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


port playFromElm : String -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



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


type Model
    = ActiveModel AnActiveModel
    | InactiveModel


type alias AnActiveModel =
    { gameTicks : Int
    , leaves : List LeafPosition
    , score : Int
    , koala : Position
    , mouthOpen : Bool
    , credits : Credits
    }


type Credits
    = NoCredits
    | CreditsWithHeight Int


initActiveModel : ( Model, Cmd Msg )
initActiveModel =
    ( ActiveModel
        { gameTicks = 0
        , leaves = []
        , score = 0
        , koala = Position (gridSize.width * cellSize.width // 2) (gridSize.height * cellSize.height - 50)
        , mouthOpen = False
        , credits = NoCredits
        }
    , Cmd.none
    )


init : () -> ( Model, Cmd Msg )
init _ =
    ( InactiveModel, Cmd.none )



-- UPDATE


type Msg
    = Tick
    | Key WhichKey
    | PlaceLeaf ( Int, LeafType )
    | StartClick


type LeafType
    = OneLeaf
    | TwoLeaves


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        InactiveModel ->
            if msg == StartClick then
                initActiveModel

            else
                ( model, Cmd.none )

        ActiveModel act_model ->
            case msg of
                Tick ->
                    let
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

                        nextMouthOpen =
                            length unseenLeaves /= length nonEatenLeaves

                        justReachedYear =
                            nextScore == year && act_model.score /= year

                        nextCredits =
                            if justReachedYear then
                                CreditsWithHeight 200

                            else
                                case act_model.credits of
                                    NoCredits ->
                                        NoCredits

                                    CreditsWithHeight height ->
                                        CreditsWithHeight (height - 5)

                        nextModel =
                            { act_model
                                | leaves = nonEatenLeaves
                                , score = nextScore
                                , mouthOpen = nextMouthOpen
                                , gameTicks = act_model.gameTicks + 1
                                , credits = nextCredits
                            }
                    in
                    ( ActiveModel nextModel
                    , Cmd.batch
                        [ if modBy 10 act_model.gameTicks == 0 then
                            generateLeaf

                          else
                            Cmd.none
                        , if justReachedYear then
                            playFromElm "assets/happy_birthday.m4a"

                          else
                            Cmd.none
                        , if act_model.score /= nextScore then
                            playFromElm "assets/nom.wav"

                          else
                            Cmd.none
                        ]
                    )

                PlaceLeaf ( pos, leafType ) ->
                    ( ActiveModel { act_model | leaves = LeafPosition pos 20 0 leafType :: act_model.leaves }, Cmd.none )

                StartClick ->
                    ( model, Cmd.none )

                Key whichKey ->
                    let
                        koala =
                            act_model.koala

                        newKoala =
                            { koala | x = onScreen (koala.x + getShift whichKey) }
                    in
                    ( ActiveModel { act_model | koala = newKoala }, Cmd.none )


onScreen : Int -> Int
onScreen x =
    min (max x screenLeft) screenRight


outOfRange : Int -> Position -> LeafPosition -> Bool
outOfRange distance koala leaf =
    abs (koala.x - leaf.x) > distance || abs (koala.y - leaf.y) > distance


outOfEatingRange : Position -> LeafPosition -> Bool
outOfEatingRange =
    outOfRange 30


outOfSeeingRange : Position -> LeafPosition -> Bool
outOfSeeingRange =
    outOfRange 100


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
    Random.generate PlaceLeaf (Random.pair (Random.int screenLeft screenRight) (Random.uniform OneLeaf [ TwoLeaves ]))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Time.every tickFrequency (\_ -> Tick)
        ]


str : Int -> String
str =
    String.fromInt


backgroundImageAnd : List (Html Msg) -> Html Msg
backgroundImageAnd details =
    svg
        [ width "100%"
        , height "auto"
        , viewBox ("0 0 " ++ String.fromInt (gridSize.width * cellSize.width) ++ " " ++ String.fromInt (gridSize.height * cellSize.height))
        , Svg.Attributes.style "touch-action: none"
        ]
        (image [ x (String.fromInt 0), y (String.fromInt 0), width (String.fromInt (gridSize.width * cellSize.width)), height (String.fromInt (gridSize.height * cellSize.height)), xlinkHref "assets/background.png" ] [] :: details)


view : Model -> Html Msg
view model =
    case model of
        InactiveModel ->
            backgroundImageAnd [ text_ [ x "200", y "60", fontSize "32", Svg.Attributes.style "fill: white", onClick StartClick ] [ text "Click on 🐨 emoji to start" ] ]

        ActiveModel act_model ->
            backgroundImageAnd
                (List.map renderLeaf act_model.leaves
                    ++ [ text_ [ x "120", y "20", Svg.Attributes.style "fill: white" ] [ text ("Score: " ++ String.fromInt act_model.score) ], text_ [ x "260", y "60", fontSize "96", Svg.Attributes.style "fill: white", onClick (Key LeftArrow) ] [ text "←" ], text_ [ x "520", y "60", fontSize "96", Svg.Attributes.style "fill: white", onClick (Key RightArrow) ] [ text "→" ] ]
                    ++ displayKoala act_model
                    -- A faster way would be to check primality once, instead of on every tick or every render
                    ++ (if isPrime act_model.score then
                            thinkPrime act_model.koala

                        else
                            []
                       )
                    ++ displayCredits act_model.credits
                )


year : Int
year =
    19


displayLine : String -> Int -> Html Msg
displayLine string height =
    text_ [ x "200", y (str height), fontSize "10", Svg.Attributes.style "fill: black" ] [ text string ]


displayCredits : Credits -> List (Html Msg)
displayCredits credits =
    case credits of
        CreditsWithHeight h ->
            [ displayLine "Happy birthday!" (280 + h)
            , displayLine "Credits" (300 + h)
            , displayLine "Jing Wang: Artistic Director, Musician, Graphic Designer" (320 + h)
            , displayLine "Theodore Ehrenborg: Programmer" (340 + h)
            , displayLine "Lots learned from https://github.com/MartinSnyder/elm-snake" (360 + h)
            , displayLine "Background sound from:" (380 + h)
            , displayLine "\"Sunrise over the Australian Outback | Didgeridoo Music and Background Ambience\"" (400 + h)
            , displayLine "https://youtu.be/h8dv8ykprf8" (420 + h)
            ]

        NoCredits ->
            []


renderLeaf : LeafPosition -> Html Msg
renderLeaf pos =
    image
        [ x (String.fromInt pos.x)
        , y (String.fromInt pos.y)
        , width "50px"
        , height "auto"
        , xlinkHref
            (if pos.leafType == OneLeaf then
                "assets/1leaf.png"

             else
                "assets/2leaves.png"
            )
        ]
        []


renderCircle : String -> Int -> Position -> Html Msg
renderCircle color radius pos =
    circle
        [ cx (String.fromInt pos.x)
        , cy (String.fromInt pos.y)
        , r (String.fromInt radius)
        , fill color
        ]
        []


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


displayKoala : AnActiveModel -> List (Html Msg)
displayKoala act_model =
    -- Always display both images so that there's no flickering
    -- when loading the second image for the first time
    let
        displayIt url =
            image [ x (String.fromInt (act_model.koala.x - 75)), y (String.fromInt (act_model.koala.y - 80)), width "150px", height "150px", xlinkHref url ] []
    in
    if act_model.mouthOpen then
        [ displayIt "assets/koala_mouth_closed.png", displayIt "assets/koala_mouth_open.png" ]

    else
        [ displayIt "assets/koala_mouth_open.png", displayIt "assets/koala_mouth_closed.png" ]


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
