module CollisionDetection exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)


type alias BoundingBox =
    { size : Vec2 -- width and height
    , offset : Vec2 -- offset from item position
    }


type alias CollisionItem =
    { position : Vec2
    , boundingBox : BoundingBox
    }


type CollisionDirection
    = Up
    | Down
    | Right
    | Left


type Collision
    = NoCollision
    | Collides CollisionDirection


checkCollision : CollisionItem -> CollisionItem -> Collision
checkCollision item1 item2 =
    let
        -- Compute min and max bounds for item1
        minX1 =
            Vec2.getX item1.position + Vec2.getX item1.boundingBox.offset

        minY1 =
            Vec2.getY item1.position + Vec2.getY item1.boundingBox.offset

        maxX1 =
            minX1 + Vec2.getX item1.boundingBox.size

        maxY1 =
            minY1 + Vec2.getY item1.boundingBox.size

        -- Compute min and max bounds for item2
        minX2 =
            Vec2.getX item2.position + Vec2.getX item2.boundingBox.offset

        minY2 =
            Vec2.getY item2.position + Vec2.getY item2.boundingBox.offset

        maxX2 =
            minX2 + Vec2.getX item2.boundingBox.size

        maxY2 =
            minY2 + Vec2.getY item2.boundingBox.size

        -- Check if bounding boxes overlap
        overlaps =
            maxX1
                > minX2
                && minX1
                < maxX2
                && maxY1
                > minY2
                && minY1
                < maxY2

        -- Determine collision direction
        direction =
            if maxY1 <= minY2 then
                Just Down

            else if minY1 >= maxY2 then
                Just Up

            else if maxX1 <= minX2 then
                Just Right

            else if minX1 >= maxX2 then
                Just Left

            else
                Nothing
    in
    if overlaps then
        case direction of
            Just d ->
                Collides d

            Nothing ->
                NoCollision

    else
        NoCollision
