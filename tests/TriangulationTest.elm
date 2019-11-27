module TriangulationTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Math.Vector2 exposing (vec2)
import Triangulation


suite : Test
suite =
    let
        a = vec2 1 1
        b = vec2 4 1
        c = vec2 4 4
        d = vec2 1 4
    in
    describe "Compgeo test suite"
    [ describe "triangulate tests"
        [ test "y-monotone rect" <|
            \_ ->
                Expect.equal (Just [(c,a)]) <| Triangulation.triangulate [a,b,c,d]
        , test "non y-monotone rect" <|
            \_ ->
                Expect.equal Nothing <| Triangulation.triangulate [a,d,b,c]
        , test "just one element" <|
            \_ ->
                Expect.equal (Just[]) <| Triangulation.triangulate [a]
        ]
    , describe "isYMonotone tests"
        [ test "test with vertices with different y values" <|
            \_ ->
                Expect.equal True <| Triangulation.isYMonotone [c,b]
        , test "test with vertices with identical y values" <|
            \_ ->
                Expect.equal True <| Triangulation.isYMonotone [a,b]
        , test "empty list" <|
            \_ ->
                Expect.equal True <| Triangulation.isYMonotone []
        , test "single element" <|
            \_ ->
                Expect.equal True <| Triangulation.isYMonotone [a]
        ]
    ]
