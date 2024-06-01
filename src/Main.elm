module Main exposing (..)
import Json.Decode as Decode
import Browser
import Browser.Events
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Random
import Grid exposing (..)
import Util exposing (..)
import NonEmptyList as NEL exposing (NonEmptyList)
import Html.Events.Extra.Pointer as Pointer

main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- CONSTANTS
gridSize = Size 40 20
cellSize = Size 20 20
tickFrequency = 100
initialSnakeLength = 20

-- MODEL
type State = Active | Inactive

type WhichKey = LeftArrow | RightArrow | OtherKey

type alias Position2 = {
        x: Int,
        y: Int,
        y_vel : Int
    }

type alias Model =
  { state : State
  , gameTicks : Int
  , direction : Direction
  , snake : NonEmptyList Position
  , prize : (Maybe Position)
  , leaves: List Position2
  , score : Int
  , highScore : Int
  , position: Int
  }

gravity = 1

initGame : State -> Int -> (Model, Cmd Msg)
initGame initialState highScore =
  let
    head = computeGridCenter gridSize
    initSnake = NonEmptyList head (List.repeat (initialSnakeLength - 1) head)
  in
  ( { state = initialState
    , gameTicks = 0
    , direction = Up
    , snake = initSnake
    , prize = Nothing
    , leaves = [Position2 1 20 0, Position2 100 20 0]
    , score = 0
    , position = 0
    , highScore = highScore
    }
  , if (initialState == Active) then placePrize initSnake else Cmd.none
  )

init : () -> (Model, Cmd Msg)
init _ = initGame Inactive 0

-- UPDATE
type Msg = Tick Time.Posix | PlacePrize (Maybe Position) | PointerDownAt ( Float, Float ) | Key WhichKey

applyGravity: Position2 -> Position2
applyGravity leaf =
    let new_y = leaf.y + leaf.y_vel
        new_y_vel = leaf.y_vel + gravity
    in
        {leaf |
           y = new_y,
           y_vel = new_y_vel}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if (model.state == Active) then
    case msg of
      PointerDownAt offsetPos ->
        ( { model | direction = pointerOffsetToDirection offsetPos model.direction model.snake.head }
        , Cmd.none
        )

      Tick time ->
        let
          nextHead = adjustPosition model.snake.head model.direction
          atePrize = (Just nextHead) == model.prize
          nextScore = if atePrize then model.score + 1 else model.score
          nextTail = model.snake.head :: if atePrize then model.snake.tail else stripLast model.snake.tail
          nextSnake = NonEmptyList nextHead nextTail
          nextState = if (isLegalState nextSnake) then Active else Inactive
          nextModel =
            { model
              | state = nextState
              , leaves = List.map applyGravity model.leaves
              , snake = nextSnake
              , score = nextScore
              , highScore = Basics.max nextScore model.highScore
              , gameTicks = if (nextState == Active) then model.gameTicks + 1 else (-1000 // tickFrequency) // 2
            }
        in
          ( nextModel , if atePrize then placePrize nextSnake else Cmd.none )

      PlacePrize  pos ->
        ( { model | prize = pos }, Cmd.none )
      Key whichKey -> ( { model | position = model.position +getShift whichKey }, Cmd.none )
    else
      case msg of
        PointerDownAt _ -> if (model.gameTicks >= 0) then initGame Active model.highScore else ( model, Cmd.none )
        Tick time ->
          ({ model | gameTicks = model.gameTicks + 1}, Cmd.none )
        Key whichKey -> ( { model | position = model.position + getShift whichKey }, Cmd.none )
        _ -> ( model, Cmd.none )

getShift : WhichKey -> Int
getShift key = let scale = 20 in
                case key of
                   LeftArrow -> -scale
                   RightArrow -> scale
                   OtherKey -> 0

isLegalState : NonEmptyList Position -> Bool
isLegalState snake = (isInGrid gridSize snake.head) && not (List.member snake.head snake.tail)

placePrize : NonEmptyList Position -> Cmd Msg
placePrize snake =
  let
    allPoints = computePointsInGrid gridSize
    snakePoints = NEL.toList snake
    validPoints = List.filter (\p -> not (List.member p snakePoints)) allPoints
  in
  Random.generate PlacePrize (Random.map (\i -> List.head (List.drop i validPoints)) (Random.int 0 (List.length validPoints - 1)))

pointerOffsetToDirection : ( Float, Float ) -> Direction -> Position -> Direction
pointerOffsetToDirection eventOffset currentDirection snakeHead =
  let
    (eventX, eventY) = eventOffset
    dx = eventX - ((toFloat snakeHead.x + 0.5) * toFloat cellSize.width)
    dy = eventY - ((toFloat snakeHead.y + 0.5) * toFloat cellSize.height)
  in
  if (currentDirection == Up || currentDirection == Down) then
    if (dx < 0) then Left else Right
  else
    if (dy < 0) then Up else Down

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Time.every tickFrequency Tick
        ]

-- VIEW
view : Model -> Html Msg
view model =
  svg [ width "100%"
      , height "auto"
      , viewBox ("0 0 " ++ String.fromInt (gridSize.width * cellSize.width) ++ " " ++ String.fromInt (gridSize.height * cellSize.height))
      , Pointer.onDown (\event -> PointerDownAt event.pointer.offsetPos)
      , Svg.Attributes.style "touch-action: none"
      ]
      (  rect [ width (String.fromInt (gridSize.width * cellSize.width)), height (String.fromInt (gridSize.height * cellSize.height))] []
      --:: (maybeToList model.prize |> List.map (\pos -> renderCircle "green" pos))
      :: []
      --++ List.map (renderCircle "red") model.snake.tail
      ++ List.map (renderCircle2 ) model.leaves
      --++ [ renderCircle "purple" model.snake.head ]
      ++ [ image [x (String.fromInt model.position), y (String.fromInt (gridSize.height * cellSize.height-50)), width "50px" ,height "50px", xlinkHref "https://upload.wikimedia.org/wikipedia/commons/4/49/Koala_climbing_tree.jpg"] [] ]
     -- ++ [ text_ [ x "5", y "20", Svg.Attributes.style "fill: white"] [ text ("Ticks: " ++ (String.fromInt model.gameTicks))]
      --   , text_ [ x (String.fromInt ((gridSize.width * cellSize.width) - 5)), y "20", Svg.Attributes.style "fill: white; text-anchor: end"] [ text ("High Score: " ++ (String.fromInt model.highScore))]
       --  ]
      -- ++ if (model.state == Inactive && model.gameTicks >= 0) then [ text_ [ x "50%", y "50%", Svg.Attributes.style "dominant-baseline:middle; text-anchor:middle; fill: white; font-size: large"] [ text "Click or touch to begin..." ] ] else []
      )

renderCircle2 : Position2 -> Html Msg
renderCircle2 pos =
  circle [ cx (String.fromInt (pos.x))
         , cy (String.fromInt (pos.y))
         , r (String.fromInt 10)
         , fill "green"
         ] []

renderCircle : String -> Position -> Html Msg
renderCircle color pos =
  circle [ cx (String.fromInt ((pos.x * cellSize.width) + (cellSize.width // 2)))
         , cy (String.fromInt ((pos.y * cellSize.height) + (cellSize.height // 2)))
         , r (String.fromInt (cellSize.height // 2))
         , fill color
         ] []

-- https://github.com/MartinSnyder/elm-snake

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" -> Key LeftArrow
        "ArrowRight" -> Key RightArrow
        _ -> Key OtherKey
