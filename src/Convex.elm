module Convex exposing (..)

import Math.Vector2 exposing (Vec2)

convexHull : List Vec2 -> List Vec2
convexHull list =
    let
        ordered = List.sortWith compare_x list
        reverse = List.reverse ordered
        upper_bound = cH_bound_wrapper ordered
        lower_bound = cH_bound_wrapper reverse
    in
        List.append upper_bound <| Maybe.withDefault [] <| List.tail lower_bound

-- Wrapper around cH_bound. Constructs and returns a vertice boundary from the passed list.
-- The passed list should be ordered by x_value: low->high for upper bound; high->low for lower bound.
cH_bound_wrapper : List Vec2 -> List Vec2
cH_bound_wrapper list =
    case list of
        [] -> []
        [a] -> [a]
        (a::xsa) -> case xsa of
            [] -> [a]
            [b] -> [a, b]
            (b::xsb) -> case xsb of
                [] -> [a, b]
                [c] -> cH_bound a b c [] []
                (c::xsc) -> cH_bound a b c [] xsc


-- Recursively calculates a single boundary of a convex hull (upper or lower)
cH_bound : Vec2 -> Vec2 -> Vec2 -> List Vec2 -> List Vec2 -> List Vec2
cH_bound a b c acc rest =
    case rightTurn a b c of
        True ->
            case rest of
                [] -> (List.reverse acc) ++ [a,b,c] -- used as stack so far, now reverse for desired order
                [x] -> cH_bound b c x (a :: acc) []
                (x::xs) -> cH_bound b c x (a :: acc) xs
        False ->
            case acc of
                [] -> case rest of
                    [] -> [a, c]
                    [x] -> cH_bound a c x [] []
                    (x::xs) -> cH_bound a c x [] xs
                [prev] -> cH_bound prev a c [] rest
                (prev::xs) -> cH_bound prev a c xs rest

-- compares the x-value / d0-value of two vertices
compare_x : Vec2 -> Vec2 -> Order
compare_x a b =
    compare (Math.Vector2.getX a) (Math.Vector2.getX b)

-- Determines whether the passed vertices form a right turn or not.
-- (Does this algorithm work only for the upper bound?)
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


