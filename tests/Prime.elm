module Prime exposing (suite)

import Expect
import Main exposing (isPrime)
import Test exposing (Test, describe, test)


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
