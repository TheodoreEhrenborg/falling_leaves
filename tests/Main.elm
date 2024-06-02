module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The isPrime module"
        [ test "works on the range -20,...,20" <|
            \_ ->
                List.range -20 20
                    |> List.filter isPrime
                    |> Expect.equal [ 2, 3, 5, 7, 11, 13, 17, 19 ]
        , test "works on squares" <|
            \_ ->
                List.range 1 10
                    |> List.map (\x -> x * x)
                    |> List.filter isPrime
                    |> Expect.equal []
        ]
