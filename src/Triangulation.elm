module Triangulation exposing (triangulate, isYMonotone)
{-|
This module exposes functions for triangulating polygons.

*Polygons* in this module are implemented as a List Vec2 that stores an ordered list of corner vertices.

*Vertices* are implemented as Vec2

*Line segments* are implemented as (Vec2, Vec2), where the Vec2 represent start and end point of the line segment.

# Commonly used

@docs triangulate

@docs isYMonotone
-}

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Array exposing (Array)
import Set exposing (Set)
import Set.Any


type alias VectorSet = Set.Any.AnySet (Float, Float) Vec2

{-|
Triangulates a polygon and returns a list of the edges of the corresponding triangles.

Only works with y-monotone polygons so far, will return `Nothing` for non-monotone polygons.
-}
triangulate : List Vec2 -> Maybe (List (Vec2, Vec2))
triangulate polygon =
    let
        (left, right) = splitAtMax polygon
    in 
        if isYMonotone left && isYMonotone right then
            Just <| diagonal_wrapper (Set.Any.fromList vecAsTuple left) (Set.Any.fromList vecAsTuple right) polygon
        else
            Nothing

-- Splits the two passed lists at the vertex with the highest y-Value
splitAtMax : List Vec2 -> (List Vec2, List Vec2)
splitAtMax polygon =
    let
        with_index = List.indexedMap (\i val -> (i,val)) polygon 
        polygon_array = Array.fromList polygon
    in
        case with_index of
            [] -> ([], [])
            [x] -> ([Tuple.second x], [Tuple.second x])
            (x::xs) ->
                -- get max y-Index
                List.foldl (\(index_a, a) (index_b, b) -> if getY a >= getY b then (index_a, a) else (index_b, b)) x xs
                |> Tuple.first
                -- split into two lists at max
                |> (\index ->   ( Array.toList <| Array.slice 0 index polygon_array
                                , Array.toList <| Array.slice index (Array.length polygon_array
                                )
                                polygon_array))


{-|
Determines if the passed List Vec2 is y-monotone.
-}
isYMonotone : List Vec2 -> Bool
isYMonotone sequence =
    let
         -- we only need the y-coordinates
        y_only = List.map (\vec -> getY vec) sequence
    in  
        case y_only of
            -- Line Sequences with 0 and 1 vertices are always y-monotone
            [] -> True
            [_] -> True
            (y::ys) ->
                case List.foldl monotonePredicate (Just y) ys of
                    Just _ -> True
                    Nothing -> False


monotonePredicate : Float -> Maybe Float -> Maybe Float
monotonePredicate x acc =
    case acc of
        Nothing -> Nothing
        Just a -> if x<a then Just x else Nothing


-- wrapper around tail recursive diagonal function
diagonal_wrapper : VectorSet -> VectorSet -> List Vec2 -> List(Vec2, Vec2)
diagonal_wrapper left right polygon =
    case polygon of
        [] -> []
        [a] -> []
        (a::xsa) -> case xsa of
            [] -> []
            [b] -> []
            (b::xsb) -> case xsb of
                [] -> []
                [c] -> diagonal a b c [] [] [] left right
                (c::xsc) -> diagonal a b c xsc [] [] left right


diagonal : Vec2 -> Vec2 -> Vec2 -> List Vec2 -> List Vec2 -> List (Vec2, Vec2) -> VectorSet -> VectorSet -> List (Vec2,Vec2)
diagonal p q r rest stack acc left right =
    let
        side =  if Set.Any.member p left then
                    left
                else right
    in
        -- TODO: check for edges outside the polygon ( => use stack)
        case rest of
            [] -> (p, q) :: acc
            [last] -> diagonal r p last  [] [] ((p, r) :: acc) left right
            (next::newRest) ->
                if Set.Any.member r side then
                    diagonal r p next  newRest [] ((p, r) :: acc) left right
                else
                    diagonal r q next newRest [] ((q, r) :: acc) left right


vecAsTuple : Vec2 -> (Float, Float)
vecAsTuple vec =
    (getX vec, getY vec)