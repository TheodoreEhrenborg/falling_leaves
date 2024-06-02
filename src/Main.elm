module Main exposing (..)

import Browser
import Browser.Events
import Grid exposing (..)
import Html exposing (Html)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as Decode
import List exposing (length)
import NonEmptyList as NEL exposing (NonEmptyList)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Time
import Util exposing (..)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- CONSTANTS


gridSize =
    Size 40 20


cellSize =
    Size 20 20


tickFrequency =
    100


initialSnakeLength =
    20



-- MODEL


type WhichKey
    = LeftArrow
    | RightArrow
    | OtherKey


type alias Position2 =
    { x : Int
    , y : Int
    , y_vel : Int
    }


type alias Model =
    { gameTicks : Int
    , direction : Direction
    , snake : NonEmptyList Position
    , prize : Maybe Position
    , leaves : List Position2
    , score : Int
    , highScore : Int
    , koala : Position2
    }


gravity =
    1


initGame : Int -> ( Model, Cmd Msg )
initGame highScore =
    let
        head =
            computeGridCenter gridSize

        initSnake =
            NonEmptyList head (List.repeat (initialSnakeLength - 1) head)
    in
    ( { gameTicks = 0
      , direction = Up
      , snake = initSnake
      , prize = Nothing
      , leaves = [ Position2 1 20 0, Position2 100 20 0 ]
      , score = 0
      , koala = Position2 0 (gridSize.height * cellSize.height - 50) 0
      , highScore = highScore
      }
    , Cmd.none
    )


init : () -> ( Model, Cmd Msg )
init _ =
    initGame 0



-- UPDATE


type Msg
    = Tick Time.Posix
    | PlacePrize (Maybe Position)
    | PointerDownAt ( Float, Float )
    | Key WhichKey
    | PlaceLeaf Int


applyGravity : Position2 -> Position2
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
        PointerDownAt offsetPos ->
            --let dummy = Debug.log "dump tuple" offsetPos in
            ( { model | direction = pointerOffsetToDirection offsetPos model.direction model.snake.head }
            , Cmd.none
            )

        Tick time ->
            let
                nextHead =
                    adjustPosition model.snake.head model.direction

                --dummy =Debug.log "time" time
                atePrize =
                    Just nextHead == model.prize

                nextTail =
                    model.snake.head
                        :: (if atePrize then
                                model.snake.tail

                            else
                                stripLast model.snake.tail
                           )

                nextSnake =
                    NonEmptyList nextHead nextTail

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
                        , snake = nextSnake
                        , score = nextScore
                        , highScore = Basics.max nextScore model.highScore
                        , gameTicks = model.gameTicks + 1
                    }
            in
            ( nextModel
            , if modBy 10 model.gameTicks == 0 then
                generateLeaf

              else
                Cmd.none
            )

        PlacePrize pos ->
            ( { model | prize = pos }, Cmd.none )

        PlaceLeaf pos ->
            ( { model | leaves = Position2 pos 20 0 :: model.leaves }, Cmd.none )

        Key whichKey ->
            let
                koala =
                    model.koala

                newKoala =
                    { koala | x = koala.x + getShift whichKey }

                --dummy = Debug.log "koala position" newKoala
            in
            ( { model | koala = newKoala }, Cmd.none )


isFar : Position2 -> Position2 -> Bool
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


isLegalState : NonEmptyList Position -> Bool
isLegalState snake =
    isInGrid gridSize snake.head && not (List.member snake.head snake.tail)


generateLeaf : Cmd Msg
generateLeaf =
    Random.generate PlaceLeaf (Random.int 0 (cellSize.width * gridSize.width))


placePrize : NonEmptyList Position -> Cmd Msg
placePrize snake =
    let
        allPoints =
            computePointsInGrid gridSize

        snakePoints =
            NEL.toList snake

        validPoints =
            List.filter (\p -> not (List.member p snakePoints)) allPoints
    in
    Random.generate PlacePrize (Random.map (\i -> List.head (List.drop i validPoints)) (Random.int 0 (List.length validPoints - 1)))


pointerOffsetToDirection : ( Float, Float ) -> Direction -> Position -> Direction
pointerOffsetToDirection eventOffset currentDirection snakeHead =
    let
        ( eventX, eventY ) =
            eventOffset

        dx =
            eventX - ((toFloat snakeHead.x + 0.5) * toFloat cellSize.width)

        dy =
            eventY - ((toFloat snakeHead.y + 0.5) * toFloat cellSize.height)
    in
    if currentDirection == Up || currentDirection == Down then
        if dx < 0 then
            Left

        else
            Right

    else if dy < 0 then
        Up

    else
        Down



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Time.every tickFrequency Tick
        ]



-- VIEW


view : Model -> Html Msg
view model =
    svg
        [ width "100%"
        , height "auto"
        , viewBox ("0 0 " ++ String.fromInt (gridSize.width * cellSize.width) ++ " " ++ String.fromInt (gridSize.height * cellSize.height))
        , Pointer.onDown (\event -> PointerDownAt event.pointer.offsetPos)
        , Svg.Attributes.style "touch-action: none"
        ]
        (rect [ width (String.fromInt (gridSize.width * cellSize.width)), height (String.fromInt (gridSize.height * cellSize.height)) ] []
            --:: (maybeToList model.prize |> List.map (\pos -> renderCircle "green" pos))
            :: []
            --++ List.map (renderCircle "red") model.snake.tail
            ++ List.map renderCircle2 model.leaves
            --++ [ renderCircle "purple" model.snake.head ]
            ++ [ image [ x (String.fromInt model.koala.x), y (String.fromInt model.koala.y), width "50px", height "50px", xlinkHref "https://upload.wikimedia.org/wikipedia/commons/4/49/Koala_climbing_tree.jpg" ] [] ]
            --            ++ [ text_ [ x "100", y "20", Svg.Attributes.style "fill: white" ] [ text ("Ticks: " ++ String.fromInt model.gameTicks) ] ]
            ++ [ text_ [ x "5", y "20", Svg.Attributes.style "fill: white" ] [ text ("Score: " ++ String.fromInt model.score) ] ]
            ++ [ text_ [ x "370", y "20", Svg.Attributes.style "fill: white", onClick (Key LeftArrow) ] [ text "←" ] ]
            ++ [ text_ [ x "430", y "20", Svg.Attributes.style "fill: white", onClick (Key RightArrow) ] [ text "→" ] ]
         --   , text_ [ x (String.fromInt ((gridSize.width * cellSize.width) - 5)), y "20", Svg.Attributes.style "fill: white; text-anchor: end"] [ text ("High Score: " ++ (String.fromInt model.highScore))]
         --  ]
         -- ++ if (model.state == Inactive && model.gameTicks >= 0) then [ text_ [ x "50%", y "50%", Svg.Attributes.style "dominant-baseline:middle; text-anchor:middle; fill: white; font-size: large"] [ text "Click or touch to begin..." ] ] else []
        )


renderCircle2 : Position2 -> Html Msg
renderCircle2 pos =
    circle
        [ cx (String.fromInt pos.x)
        , cy (String.fromInt pos.y)
        , r (String.fromInt 10)
        , fill "green"
        ]
        []


renderCircle : String -> Position -> Html Msg
renderCircle color pos =
    circle
        [ cx (String.fromInt ((pos.x * cellSize.width) + (cellSize.width // 2)))
        , cy (String.fromInt ((pos.y * cellSize.height) + (cellSize.height // 2)))
        , r (String.fromInt (cellSize.height // 2))
        , fill color
        ]
        []



-- https://github.com/MartinSnyder/elm-snake


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            Key LeftArrow

        "ArrowRight" ->
            Key RightArrow

        _ ->
            Key OtherKey
