module Grid exposing (Direction(..), Offset, Position, Size, adjustPosition, computeGridCenter)


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Size =
    { width : Int
    , height : Int
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias Offset =
    { dx : Int
    , dy : Int
    }


directionToOffset : Direction -> Offset
directionToOffset dir =
    case dir of
        Up ->
            Offset 0 -1

        Down ->
            Offset 0 1

        Left ->
            Offset -1 0

        Right ->
            Offset 1 0


applyOffset : Position -> Offset -> Position
applyOffset pos offset =
    Position (pos.x + offset.dx) (pos.y + offset.dy)


adjustPosition : Position -> Direction -> Position
adjustPosition pos dir =
    applyOffset pos (directionToOffset dir)


computeGridCenter : Size -> Position
computeGridCenter size =
    Position (size.width // 2) (size.height // 2)
