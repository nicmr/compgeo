module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Math.Vector2
import Convex


suite : Test
suite =
    describe "Compgeo test suite"
    [   describe "right_turn tests"
        [ test "Should be recognised as a right curve" <|
            \_ ->
                let
                    a = Math.Vector2.vec2 0 0
                    b = Math.Vector2.vec2 2 1
                    c = Math.Vector2.vec2 4 0
                in
                    Expect.equal True (Convex.right_turn a b c)

        , test "Should not be recognised as a right curve" <|
            \_ ->
                let
                    a = Math.Vector2.vec2 0 0
                    b = Math.Vector2.vec2 2 1
                    c = Math.Vector2.vec2 4 10
                in
                    Expect.equal False (Convex.right_turn a b c)
        , test "Third Vector is directly on the vector a->b. Should be recognised as a right curve." <|
            \_ ->
                let
                    a = Math.Vector2.vec2 0 0
                    b = Math.Vector2.vec2 2 1
                    c = Math.Vector2.vec2 4 2
                in
                    Expect.equal True (Convex.right_turn a b c)
        ]
    ]
        
