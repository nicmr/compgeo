module Convex exposing (..)

import Math.Vector2 exposing (Vec2)

convexHull : List Vec2 -> List Vec2
convexHull list =
    let
        ordered = List.sortWith compare_x list
        reverse = List.reverse ordered
    in
        --placeholder
        [(Math.Vector2.vec2 1 2)]


-- upper : List Vec2 -> List Vec2
-- upper list =



cH_bound_wrapper : List Vec2 -> List Vec2
cH_bound_wrapper list =
    case list of
        [] -> []
        [x] -> [x]
        (a::xsa) -> case xsa of
            [] -> [a]
            [x] -> [a, x]
            (b::xsb) -> case xsb of
                [] -> [a, b]
                [x] -> [a, b, x]
                (c::xsc) -> cH_bound a b c [] xsc


-- Recursively calculates a single boundary of a convex hull (upper or lower)
cH_bound : Vec2 -> Vec2 -> Vec2 -> List Vec2 -> List Vec2 -> List Vec2
cH_bound a b c acc rest =
    case rightTurn a b c of
        True ->
            case rest of
                [] -> List.reverse acc -- used as stack so far, now reverse for desired order
                [x] -> cH_bound b c x (a :: acc) []
                (x::xs) -> cH_bound b c x (a :: acc) xs
        False ->
            case acc of
                [] -> []
                [x] -> cH_bound a b x [] rest
                (x::xs) -> cH_bound a b x xs rest





compare_x : Vec2 -> Vec2 -> Order
compare_x a b =
    compare (Math.Vector2.getX a) (Math.Vector2.getX b)

rightTurn : Vec2 -> Vec2 -> Vec2 -> Bool
rightTurn a b c =
    let
        slope = Math.Vector2.sub b a
        scale = (Math.Vector2.sub c a |> Math.Vector2.getX ) / Math.Vector2.getX(slope)
        expected = Math.Vector2.add a ( Math.Vector2.scale scale slope)
    in
        case compare (Math.Vector2.getY c) (Math.Vector2.getY expected) of
            LT -> True
            EQ -> True
            GT -> False


