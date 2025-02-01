module Player exposing (..)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import CollisionDetection exposing (BoundingBox)
import Color
import Keys exposing (Keys)
import Math.Vector2 as Vec2 exposing (Vec2, direction, vec2)
import Math.Vector4 exposing (vec4)
import Types exposing (Camera, Power, toPoint)


type Direction
    = Left
    | Right


type alias Player =
    { position : Vec2
    , boundingBox : BoundingBox
    , direction : Direction
    , moving : Bool
    , powers : List Power
    }


init : Player
init =
    Player (vec2 50 300)
        (BoundingBox
            (vec2 0 0)
            (vec2 0 0)
        )
        Right
        False
        []


addPower : Player -> Power -> Player
addPower player power =
    { player
        | powers = power :: player.powers
    }


updatePlayer : Float -> Keys -> Player -> Player
updatePlayer dt keys person =
    let
        y =
            if keys.up then
                -1

            else
                0
                    + (if keys.down then
                        1

                       else
                        0
                      )

        x =
            if keys.right then
                1

            else
                0
                    + (if keys.left then
                        -1

                       else
                        0
                      )

        position =
            Vec2.add person.position (vec2 x y)
    in
    { person
        | position = position
        , direction =
            if x == 1 then
                Left

            else if x == -1 then
                Right

            else
                person.direction
        , moving =
            case ( x, y ) of
                ( 0, 0 ) ->
                    False

                _ ->
                    True
    }


renderPlayer : Float -> Player -> Camera -> Renderable
renderPlayer frame player camera =
    let
        ( x, y ) =
            toPoint player.position

        ( xOffset, yOffset ) =
            Types.getCameraOffsets camera

        speedFactor =
            10

        amplitude =
            5

        legOffset =
            amplitude * sin (frame / speedFactor)

        bodyOffset =
            1 * sin (frame / 20)
    in
    group []
        [ shapes
            [ fill Color.black
            ]
            [ case player.direction of
                Left ->
                    circle ( 5 + x + xOffset, 5 + y + yOffset + bodyOffset ) 20

                Right ->
                    circle ( 0 + x + xOffset, 5 + y + yOffset + bodyOffset ) 20
            , if player.moving then
                rect ( 5 + x + xOffset, 10 + y + legOffset + yOffset ) 10 30

              else
                rect ( 5 + x + xOffset, 13 + y + yOffset ) 10 30
            , if player.moving then
                rect ( -10 + x + xOffset, 10 + y - legOffset + yOffset ) 10 30

              else
                rect ( -10 + x + xOffset, 13 + y + yOffset ) 10 30
            ]
        , shapes
            [ fill Color.white ]
            [ case player.direction of
                Left ->
                    circle ( 15 + x + xOffset, 5 + y + yOffset + bodyOffset ) 4

                Right ->
                    circle ( -15 + x + xOffset, 5 + y + yOffset + bodyOffset ) 4
            , case player.direction of
                Left ->
                    circle ( 5 + x + xOffset, 5 + y + yOffset + bodyOffset ) 5

                Right ->
                    circle ( -5 + x + xOffset, 5 + y + yOffset + bodyOffset ) 5
            ]
        ]
