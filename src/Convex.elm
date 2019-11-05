module Convex exposing (convexHull)

import Math.Vector2 exposing (Vec2)

type alias TurnPredicate = (Vec2 -> Vec2 -> Vec2 -> Bool)
type Mode = Upper | Lower

-- Constructs a convex hull of vectors in the passed list.
-- Returns an ordered list of the points on the hull boundary, starting with the point of the lowest x Value.
-- The last point will be identical to the first.
convexHull : List Vec2 -> List Vec2
convexHull list =
    let
        upper_bound = boundWrapper Upper list
        lower_bound = boundWrapper Lower list
    in
        List.append upper_bound <| Maybe.withDefault [] <| List.tail lower_bound

-- Wrapper around bound. Constructs and returns a vertice boundary from the passed list.
boundWrapper: Mode -> List Vec2 -> List Vec2
boundWrapper mode list =
    let
        increasing = List.sortWith compare_x list

        (predicate, ordered) = case mode of
            Upper -> (rightTurnUpper, increasing)
            Lower -> (rightTurnLower, List.reverse increasing)
    in  
        case ordered of
            [] -> []
            [a] -> [a]
            (a::xsa) -> case xsa of
                [] -> [a]
                [b] -> [a, b]
                (b::xsb) -> case xsb of
                    [] -> [a, b]
                    [c] -> bound predicate a b c [] []
                    (c::xsc) -> bound predicate a b c [] xsc


-- Recursively calculates a single boundary of a convex hull,
-- ensuring any given 3 consequitive points form a right turn in accordnace with the passed predicate.
bound : TurnPredicate -> Vec2 -> Vec2 -> Vec2 -> List Vec2 -> List Vec2 -> List Vec2
bound predicate a b c acc rest =
    let
        bound_pred = bound predicate
    in 
        case predicate a b c of
            True ->
                case rest of
                    [] -> (List.reverse acc) ++ [a,b,c] -- used as stack so far, now reverse for desired order
                    [x] -> bound_pred b c x (a :: acc) []
                    (x::xs) -> bound_pred b c x (a :: acc) xs
            False ->
                case acc of
                    [] -> case rest of
                        [] -> [a, c]
                        [x] -> bound_pred a c x [] []
                        (x::xs) -> bound_pred a c x [] xs
                    [prev] -> bound_pred prev a c [] rest
                    (prev::xs) -> bound_pred prev a c xs rest

-- compares the x-value / d0-value of two vertices
compare_x : Vec2 -> Vec2 -> Order
compare_x a b =
    compare (Math.Vector2.getX a) (Math.Vector2.getX b)

-- Determines whether the passed vertices form a right turn or not.
rightTurnUpper : Vec2 -> Vec2 -> Vec2 -> Bool
rightTurnUpper a b c =
    let
        slope = Math.Vector2.sub b a
        scale = (Math.Vector2.sub c a |> Math.Vector2.getX ) / Math.Vector2.getX(slope)
        expected = Math.Vector2.add a ( Math.Vector2.scale scale slope)
    in
        case compare (Math.Vector2.getY c) (Math.Vector2.getY expected) of
            LT -> True
            EQ -> True
            GT -> False

-- Determines whether the passed vertices form a right turn or not.
rightTurnLower : Vec2 -> Vec2 -> Vec2 -> Bool
rightTurnLower a b c =
    let
        slope = Math.Vector2.sub b a
        scale = (Math.Vector2.sub c a |> Math.Vector2.getX ) / Math.Vector2.getX(slope)
        expected = Math.Vector2.add a ( Math.Vector2.scale scale slope)
    in
        case compare (Math.Vector2.getY c) (Math.Vector2.getY expected) of
            LT -> False
            EQ -> True
            GT -> True