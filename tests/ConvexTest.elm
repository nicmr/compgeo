module ConvexTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Math.Vector2
import Convex


suite : Test
suite =
    let
        a = Math.Vector2.vec2 0 0
        b = Math.Vector2.vec2 2 1
        c = Math.Vector2.vec2 4 0
        c_greater = Math.Vector2.vec2 4 10 -- use this to produce convex hulls where b is not on the upper bound
    in
        describe "Compgeo test suite"
        [ describe "convexHull tests"
            [ test "Convex hull of a triangle with vertices ordered by x value" <|
                \_ ->
                    Expect.equal [a,c_greater,b,a] <| Convex.convexHull [a,b,c_greater]
            
            , test "Convex hull of a triangle with unordered vertices" <|
                \_ ->
                    Expect.equal [a,c_greater,b,a] <| Convex.convexHull [b,a,c_greater]
            ]
    ]
        
