module ConvexTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Math.Vector2
import Convex


suite : Test
suite =
    let a = Math.Vector2.vec2 0 0
        b = Math.Vector2.vec2 2 1
        c = Math.Vector2.vec2 4 0
    in
        describe "Compgeo test suite"
        [   describe "rightTurn tests"
            [ test "Should be recognised as a right turn" <|
                \_ ->
                    Expect.equal True (Convex.rightTurn a b c)

            , test "Should not be recognised as a right turn" <|
                \_ ->
                    let
                        c_greater = Math.Vector2.vec2 4 10
                    in
                        Expect.equal False (Convex.rightTurn a b c_greater)
            , test "Third Vector is on the vector a->b. Should be recognised as a right turn." <|
                \_ ->
                    let
                        c_on_vec = Math.Vector2.vec2 4 2
                    in
                        Expect.equal True (Convex.rightTurn a b c_on_vec)
            ]

        , describe "ch_bound_wrapper tests"
            [ test "Empty lists should return empty lists" <|
                \_ ->
                    Expect.equal [] <| Convex.cH_bound_wrapper []

            , describe "Lists with 1-2 elements should return the list as is."
                [ test "Test with 1 element." <|
                    \_ ->
                        Expect.equal [a] <| Convex.cH_bound_wrapper [a]
                , test "Test with 2 elements." <|
                    \_ ->
                        Expect.equal [a, b] <| Convex.cH_bound_wrapper [a, b]
                ]
            , describe "Lists with >= 3 elements should return the bound of the convex hull" 
                [   test "Test with 3 elements where all vertices are on the hull bound." <|
                    \_ ->
                        Expect.equal [a, b, c] <| Convex.cH_bound_wrapper [a, b, c]
                ,   test "Test with 3 elements where only two vertices form the hull bound" <|
                    \_ ->
                        let
                            c_greater = Math.Vector2.vec2 4 10
                        in 
                            Expect.equal [a,c_greater] <| Convex.cH_bound_wrapper[a,b,c_greater]
                , test "Test with >3 elements" <|
                    \_ ->
                        let
                            d = Math.Vector2.vec2 5 2
                            e = Math.Vector2.vec2 7 1
                            f = Math.Vector2.vec2 9 2
                        in
                            Expect.equal [a,b,d,f] <| Convex.cH_bound_wrapper [a,b,c,d,e,f]
                ]
            ]
        , describe "cH_bound tests"
            [ test "Passing an empty rest list and points that form a right turn should return a list of the passed points." <|
                \_ ->
                    Expect.equal [a,b,c] <| Convex.cH_bound a b c [] []
            , test "Passing an empty rest list and points that do not form a right turn should return a list of only a and c." <|
                \_ ->
                    let
                        c_greater = Math.Vector2.vec2 4 10
                    in
                        Expect.equal [a,c_greater] <| Convex.cH_bound a b c_greater [] []
            ]
        , describe "convexHull tests"
            [ test "Convex hull of a triangle with vertices ordered by x value" <|
                \_ ->
                    Expect.equal [a,c,b,a] <| Convex.convexHull [a,b,c]
            
            , test "Convex hull of a triangle with unordered vertices" <|
                \_ ->
                    Expect.equal [a,c,b,a] <| Convex.convexHull [b,a,c]
            ]
    ]
        
