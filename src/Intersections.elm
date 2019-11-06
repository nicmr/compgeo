module Intersections exposing (..)
{-| This module provides you with the necessary functions to determine intersections of line segments.
A line segment is defined as a tuple of two vectors, which mark the ends of a line segment.
-}

import Math.Vector2 exposing (Vec2, getX, getY)


type Orientation = Colinear | Clockwise | Counterclockwise

-- Determines whether two Line segments intersect or not.
-- TODO: stop ignoring colinear cases
intersect : (Vec2, Vec2) -> (Vec2, Vec2) -> Bool
intersect (a1, a2) (b1, b2) =
    -- (parantheses for readability)
    (orientation a1 b1 b2 /= orientation a2 b1 b2) && (orientation a1 a2 b1 /= orientation a1 a2 b2)


-- Determines the orientation of a triangle through the passed vertices in the given order.
orientation : Vec2 -> Vec2 -> Vec2 -> Orientation
orientation p q r =
    let
        slope_combination = (getY q - getY p) * (getX r - getX q) - (getX q - getX p) * (getY r - getY q)
    in 
        if slope_combination == 0 then
            Colinear
        else if slope_combination > 0 then
            Clockwise
        else
            Counterclockwise