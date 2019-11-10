module Triangulation exposing (..)


import Math.Vector2 exposing (Vec2, getX, getY)
import Array exposing (Array)
import Set exposing (Set)

-- Polygons are modelled as List Vec2 that stores an ordered list of corner vertices.

-- Determines if the passed polygon is y-monotone
isYMonotone : List Vec2 -> Bool
isYMonotone polygon =
    let
        with_index = List.indexedMap (\i val -> (i,val)) polygon 
        polygon_array = Array.fromList polygon
    in
        case with_index of
            -- Polygons with 0 and 1 corners are always y-monotone
            [] -> True
            [x] -> True
            (x::xs) ->
                -- get max y-Index
                List.foldl (\(index_a, a) (index_b, b) -> if getY a >= getY b then (index_a, a) else (index_b, b)) x xs
                |> Tuple.first
                -- split into two at max
                |> (\index -> [Array.slice 0 index polygon_array, Array.slice index (Array.length polygon_array) polygon_array])
                |> List.map (\arr -> Array.toList arr)
                |> List.map (\sublist -> List.map (\vec2 -> getY vec2) sublist)
                |> List.map (
                    \sublist ->
                        case sublist of
                            [] -> True
                            [_] -> True
                            -- (x::xs) -> List.foldl monotonePredicate AccNothing xs
                            (y::ys) ->
                                case List.foldl monotonePredicate (Just y) ys of
                                    Just _ -> True
                                    Nothing -> False
                    )
                |> List.foldl (\z acc -> (&&) z acc) True


monotonePredicate : number -> Maybe number -> Maybe number
monotonePredicate x acc =
    case acc of
        Nothing -> Nothing
        Just a -> if x<a then Just x else Nothing

-- diagonal : Vec2 -> Vec2 -> Vec2 -> List Vec2 -> List Vec2 -> List (Vec2, Vec2) -> Set Vec2 -> Set Vec2 -> List Vec2
-- diagonal p q r rest stack acc left right =
--     let
--         side =  if Set.member p left then
--                     left
--                 else right
--     in
--         -- TODO: check for edges outside the polygon, use stack
--         case rest of
--             [] -> (p, q) :: acc
--             [last] -> diagonal r p last  [] [] ((p, r) :: acc) left right
--             (next::newRest) ->
--                 if Set.member r side then
--                     diagonal r p next  newRest [] ((p, r) :: acc) left right
--                 else
--                     diagonal r q next newRest [] ((q, r) :: acc) left right