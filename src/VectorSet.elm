module VectorSet exposing
    ( VectorSet, member, insert, fromList )

import Dict exposing (Dict)
import Set exposing (Set)
import Math.Vector2 exposing (Vec2, getX, getY)


-- Set of vectors implementation with O(dimension * O(Set)) lookup
-- The resulting complexity is O(dimension * log (n) * log(collisions)),
--      which is a lot better than O(n) in basically all applications.
-- Does not support remove operations
-- Elm really needs typeclasses :(


type alias VectorSet = Dict Float (Set Float)


empty : VectorSet
empty = 
    Dict.empty

insert : Vec2 -> VectorSet -> VectorSet
insert vec old =
    let
        x = getX vec
        y = getY vec
    in 
        case Dict.get x old of
            Just ys -> 
                Dict.insert x (Set.insert y ys) old
            Nothing ->
                Set.singleton y
                |> (\new_ys -> Dict.insert x new_ys old)


member : Vec2 -> VectorSet -> Bool
member vec set =
    let
        x = getX vec
        y = getY vec
    in
        case Dict.get x set of
            Just ys ->
                Set.member y ys
            Nothing ->
                False


fromList : List Vec2 -> VectorSet
fromList list = 
    List.foldl insert empty list
      