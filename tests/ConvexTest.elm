module ConvexTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Math.Vector2
import Convex


suite : Test
suite =
    describe "Compgeo test suite"
    [   describe "rightTurn tests"
        [ test "Should be recognised as a right turn" <|
            \_ ->
                let
                    a = Math.Vector2.vec2 0 0
                    b = Math.Vector2.vec2 2 1
                    c = Math.Vector2.vec2 4 0
                in
                    Expect.equal True (Convex.rightTurn a b c)

        , test "Should not be recognised as a right turn" <|
            \_ ->
                let
                    a = Math.Vector2.vec2 0 0
                    b = Math.Vector2.vec2 2 1
                    c = Math.Vector2.vec2 4 10
                in
                    Expect.equal False (Convex.rightTurn a b c)
        , test "Third Vector is directly on the vector a->b. Should be recognised as a right turn." <|
            \_ ->
                let
                    a = Math.Vector2.vec2 0 0
                    b = Math.Vector2.vec2 2 1
                    c = Math.Vector2.vec2 4 2
                in
                    Expect.equal True (Convex.rightTurn a b c)
        ]

    , describe "ch_bound_wrapper tests"
        [ test "Empty lists should return empty lists" <|
            \_ ->
                Expect.equal [] <| Convex.cH_bound_wrapper []

        , describe "Lists with 1-3 elements should return the list."
            [ test "Test with 1 element." <|
                \_ ->
                    let
                        a = Math.Vector2.vec2 0 0
                    in
                        Expect.equal [a] <| Convex.cH_bound_wrapper [a]
            , test "Test with 2 elements." <|
                \_ ->
                    let
                        a = Math.Vector2.vec2 0 0
                        b = Math.Vector2.vec2 2 1
                    in
                        Expect.equal [a, b] <| Convex.cH_bound_wrapper [a, b]
            , test "Test with 3 elements." <|
                \_ ->
                    let
                        a = Math.Vector2.vec2 0 0
                        b = Math.Vector2.vec2 2 1
                        c = Math.Vector2.vec2 4 0
                    in
                        Expect.equal [a, b,c ] <| Convex.cH_bound_wrapper [a, b, c]
            ]
        , test "Lists with > 3 elements should return the bound of the convex hull" <|
            \_ ->
                let
                    a = Math.Vector2.vec2 0 0
                    b = Math.Vector2.vec2 2 1
                    c = Math.Vector2.vec2 4 0
                    d = Math.Vector2.vec2 5 2
                    e = Math.Vector2.vec2 7 1
                    f = Math.Vector2.vec2 9 2
                in
                    Expect.equal [a,b,d,f] <| Convex.cH_bound_wrapper [a,b,c,d,e,f]
        ]
    , describe "cH_bound tests"
        [ test "Passing an empty rest list should return a list of only the passed points" <|
            \_ ->
                let
                    a = Math.Vector2.vec2 0 0
                    b = Math.Vector2.vec2 2 1
                    c = Math.Vector2.vec2 4 0
                in
                    Expect.equal [a,b,c] <| Convex.cH_bound a b c [] []
        
        
        ]
    ]
        
