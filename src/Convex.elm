module Convex exposing (..)

import Math.Vector2 exposing (Vec2)

convex : List Vec2 -> List Vec2
convex list =
    let
        ordered = List.sortWith compare_x list
        reverse = List.reverse ordered
    in
        --placeholder
        [(Math.Vector2.vec2 1 2)]


compare_x : Vec2 -> Vec2 -> Order
compare_x a b =
    compare (Math.Vector2.getX a) (Math.Vector2.getX b)

right_turn : Vec2 -> Vec2 -> Vec2 -> Bool
right_turn a b c =
    let
        slope = Math.Vector2.sub b a
        scale = (Math.Vector2.sub c a |> Math.Vector2.getX ) / Math.Vector2.getX(slope)
        expected = Math.Vector2.add a ( Math.Vector2.scale scale slope)
    in
        case compare (Math.Vector2.getY c) (Math.Vector2.getY expected) of
            LT -> True
            EQ -> True
            GT -> False


