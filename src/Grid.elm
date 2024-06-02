module Grid exposing (Direction(..), Position, Size)


type Direction
    = Up


type alias Size =
    { width : Int
    , height : Int
    }


type alias Position =
    { x : Int
    , y : Int
    }
