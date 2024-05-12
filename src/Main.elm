module Main exposing (main)
import Playground exposing (Time, triangle, green, moveX, moveY, rotate, game)
import Time exposing (posixToMillis)

main =
  game view update [Leaf 0 0, Leaf 0 100]

view computer memory =
  List.map (\leaf ->  triangle green 40
      |> moveX leaf.x
      |> moveY leaf.y
      |> rotate 180
   ) memory

type alias Leaf =
  { x : Float
  , y : Float
  }

update computer memory =
  memory ++ newLeaf computer.time

newLeaf : Playground.Time -> List Int
newLeaf foo =  foo --if time modBy 10 == 0 then [Leaf (Time.posixToMillis posix) 0] else []


-- See https://package.elm-lang.org/packages/evancz/elm-playground/latest/Playground
