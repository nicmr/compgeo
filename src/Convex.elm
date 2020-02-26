module Convex exposing (convexHull)
{-| This library provides you with the ability to create convex hulls from a list of vertices.
# Commonly used
@docs convexHull
-}

import Math.Vector2 exposing (Vec2, getY, getX)

{-| Constructs a convex hull of vectors in the passed list.
The returned list will be an ordered list of the points on the hull boundary, starting with the point of the lowest x Value.
The last point will be identical to the first.
Uses Andrew's algorithm with a complexity of O (n log n)
-} 
convexHull : List Vec2 -> List Vec2
convexHull vertices =
    let
        sortedByX = List.sortWith compare_x vertices
        upper = constructChain sortedByX
        lower = constructChain <| List.reverse sortedByX
    in
        List.append upper <| Maybe.withDefault [] <| List.tail lower

compare_x : Vec2 -> Vec2 -> Order
compare_x a b =
    compare (Math.Vector2.getX a) (Math.Vector2.getX b)

constructChain : List Vec2 -> List Vec2
constructChain vertices =
    case vertices of
       [] -> []
       [a] -> [a]
       [a,b] -> [a,b]
       a::b::c::xs -> chainRecursive a b c [] xs

-- Recursively calculates a single boundary of a convex hull,
-- ensuring any given 3 consequitive points form a right turn in accordnace with the passed predicate.
chainRecursive : Vec2 -> Vec2 -> Vec2 -> List Vec2 -> List Vec2 -> List Vec2
chainRecursive a b c acc rest =
    if isRightTurn a b c then
        case rest of
            [] -> List.reverse acc ++ [a,b,c] -- used as stack so far, now reverse for desired order
            [x] -> chainRecursive b c x (a :: acc) []
            (x::xs) -> chainRecursive b c x (a :: acc) xs
    else
        case acc of
            [] -> case rest of
                [] -> [a, c]
                [x] -> chainRecursive a c x [] []
                (x::xs) -> chainRecursive a c x [] xs
            [prev] -> chainRecursive prev a c [] rest
            (prev::xs) -> chainRecursive prev a c xs rest

isRightTurn : Vec2 -> Vec2 -> Vec2 -> Bool
isRightTurn a b c =
    case orientation a b c of
       Clockwise -> True
       _ -> False

type Orientation = Clockwise | Counterclockwise | Colinear

orientation : Vec2 -> Vec2 -> Vec2 -> Orientation
orientation a b c =
    let
        slope_a_b = ( getY b - getY a ) / ( getX b - getX a)
        slope_b_c = ( getY c - getY b ) / ( getX c - getX b)
    in
        case compare slope_a_b slope_b_c of
           LT -> Counterclockwise
           GT -> Clockwise
           EQ -> Colinear