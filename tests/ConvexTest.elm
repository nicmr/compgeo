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
        boundWrapperUpper = Convex.boundWrapper Convex.rightTurnUpper
    in
        describe "Compgeo test suite"
        [   describe "rightTurnUpper tests"
            [ test "Should be recognised as a right turn" <|
                \_ ->
                    Expect.equal True (Convex.rightTurnUpper a b c)

            , test "Should not be recognised as a right turn" <|
                \_ ->
                    Expect.equal False (Convex.rightTurnUpper a b c_greater)
            , test "Third Vector is on the vector a->b. Should be recognised as a right turn." <|
                \_ ->
                    let
                        c_on_vec = Math.Vector2.vec2 4 2
                    in
                        Expect.equal True (Convex.rightTurnUpper a b c_on_vec)
            ]

        , describe "boundWrapperUpper tests"
            [ test "Empty lists should return empty lists" <|
                \_ ->
                    Expect.equal [] <| boundWrapperUpper []

            , describe "Lists with 1-2 elements should return the list as is."
                [ test "Test with 1 element." <|
                    \_ ->
                        Expect.equal [a] <| boundWrapperUpper [a]
                , test "Test with 2 elements." <|
                    \_ ->
                        Expect.equal [a, b] <| boundWrapperUpper [a, b]
                ]
            , describe "Lists with >= 3 elements should return the bound of the convex hull" 
                [   test "Test with 3 elements where all vertices are on the hull bound." <|
                    \_ ->
                        Expect.equal [a, b, c] <| boundWrapperUpper [a, b, c]
                ,   test "Test with 3 elements where only two vertices form the hull bound" <|
                    \_ ->
                        Expect.equal [a,c_greater] <| boundWrapperUpper [a,b,c_greater]
                , test "Test with >3 elements" <|
                    \_ ->
                        let
                            d = Math.Vector2.vec2 5 2
                            e = Math.Vector2.vec2 7 1
                            f = Math.Vector2.vec2 9 2
                        in
                            Expect.equal [a,b,d,f] <| boundWrapperUpper [a,b,c,d,e,f]
                , test "Test with >3 elements for lower bound (inverse x-order)" <| 
                    \_ ->
                        let
                            d = Math.Vector2.vec2 5 2
                            e = Math.Vector2.vec2 7 1
                            f = Math.Vector2.vec2 9 2
                        in
                            Expect.equal [f,d,b,a] <| boundWrapperUpper <| List.reverse [a,b,c,d,e,f]
                ]
            ]
        , describe "bound tests"
            [ test "Passing an empty rest list and points that form a right turn should return a list of the passed points." <|
                \_ ->
                    Expect.equal [a,b,c] <| Convex.bound Convex.rightTurnUpper a b c [] []
            , test "Passing an empty rest list and points that do not form a right turn should return a list of only a and c." <|
                \_ ->
                    Expect.equal [a,c_greater] <| Convex.bound Convex.rightTurnUpper a b c_greater [] []
            ]
        , describe "convexHull tests"
            [ test "Convex hull of a triangle with vertices ordered by x value" <|
                \_ ->
                    Expect.equal [a,c_greater,b,a] <| Convex.convexHull [a,b,c_greater]
            
            , test "Convex hull of a triangle with unordered vertices" <|
                \_ ->
                    Expect.equal [a,c_greater,b,a] <| Convex.convexHull [b,a,c_greater]
            ]
    ]
        
