module Types exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, getX, vec2)


type alias Power =
    { title : String
    , description : String
    , cost : Int
    , effect : PowerEffect
    , id : Int
    }


type PowerEffect
    = NoEffect
    | LowerDefence
    | BuffAttack
    | BuffDefence
    | DoubleDefence
    | DoubleAttack
    | IncreaseRolls
    | DestoryEnemyTurn


toPoint : Vec2 -> ( Float, Float )
toPoint v2 =
    ( Vec2.getX v2, Vec2.getY v2 )


type alias Camera =
    { position : Vec2
    , halfViewportWidth : Float
    , halfViewportHeight : Float
    , cameraCenter : Vec2
    }


getPosWithCamera : Vec2 -> Camera -> ( Float, Float )
getPosWithCamera pos camera =
    let
        cameraOffset =
            Vec2.sub camera.position camera.cameraCenter

        negateOffset =
            Vec2.negate cameraOffset

        positionOffset =
            Vec2.add negateOffset pos
    in
    toPoint positionOffset


getCameraOffsets : Camera -> ( Float, Float )
getCameraOffsets camera =
    let
        ( x, y ) =
            toPoint camera.position
    in
    ( -(x - camera.halfViewportWidth), -(y - camera.halfViewportHeight) )
