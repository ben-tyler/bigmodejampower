module Cities exposing (..)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import CollisionDetection exposing (BoundingBox)
import Color
import Math.Vector2 as Vec2 exposing (Vec2, getX, vec2)
import Types exposing (Camera, Power, PowerEffect(..), toPoint)


initPower : Int -> PowerEffect -> Power
initPower id powerEffect =
    case powerEffect of
        NoEffect ->
            { title = "No Cigar"
            , description = "A useless power, does nothing"
            , cost = 3
            , effect = NoEffect
            , id = id
            }

        LowerDefence ->
            { title = "Complete Sabatarge"
            , description = "Lowers the defence of a power plant to 1"
            , cost = 3
            , effect = LowerDefence
            , id = id
            }

        BuffAttack ->
            { title = "Power Up"
            , description = "Increase your base attack by 2"
            , cost = 3
            , effect = BuffAttack
            , id = id
            }

        BuffDefence ->
            { title = "Power Defence"
            , description = "Increase your defence by 2"
            , cost = 3
            , effect = NoEffect
            , id = id
            }

        DoubleDefence ->
            { title = "Total Power Dedence"
            , description = "Double your current defence"
            , cost = 3
            , effect = DoubleDefence
            , id = id
            }

        DoubleAttack ->
            { title = "Total Power Buff"
            , description = "Double your current base attack"
            , cost = 3
            , effect = DoubleAttack
            , id = id
            }

        IncreaseRolls ->
            { title = "Reroll Power"
            , description = "Increase the amount of roles your have left"
            , cost = 3
            , effect = IncreaseRolls
            , id = id
            }

        DestoryEnemyTurn ->
            { title = "Suck Powerplant Power"
            , description = "Nullify the enemies turn"
            , cost = 3
            , effect = DestoryEnemyTurn
            , id = id
            }


initCities : List City
initCities =
    [ City
        (vec2 100 250)
        (BoundingBox
            (vec2 50 50)
            (vec2 0 0)
        )
        PlayerOwned
        1
        ( 0, 0 )
        ( 0, 0 )
        [ initPower 1 NoEffect
        , initPower 2 DestoryEnemyTurn
        , initPower 3 IncreaseRolls
        ]
    , City (vec2 500 300)
        (BoundingBox
            (vec2 0 0)
            (vec2 0 0)
        )
        EnemyOwned
        2
        ( 1, 2 )
        ( 0, 1 )
        [ initPower 4 BuffDefence
        , initPower 5 BuffAttack
        , initPower 6 DoubleDefence
        ]
    , City (vec2 300 280)
        (BoundingBox
            (vec2 0 0)
            (vec2 0 0)
        )
        EnemyOwned
        3
        ( 1, 3 )
        ( 0, 2 )
        [ initPower 7 LowerDefence
        , initPower 8 BuffAttack
        , initPower 9 BuffAttack
        ]
    , City (vec2 600 50)
        (BoundingBox
            (vec2 0 0)
            (vec2 0 0)
        )
        EnemyOwned
        4
        ( 2, 4 )
        ( 0, 4 )
        [ initPower 10 BuffDefence
        , initPower 11 BuffAttack
        , initPower 12 DoubleAttack
        ]
    , City (vec2 800 400)
        (BoundingBox
            (vec2 0 0)
            (vec2 0 0)
        )
        EnemyOwned
        5
        ( 3, 6 )
        ( 2, 5 )
        []
    , City (vec2 500 800)
        (BoundingBox
            (vec2 0 0)
            (vec2 0 0)
        )
        EnemyOwned
        6
        ( 4, 8 )
        ( 5, 8 )
        []
    ]


type Owner
    = PlayerOwned
    | EnemyOwned


type alias City =
    { position : Vec2
    , boundingBox : BoundingBox
    , owner : Owner
    , id : Int
    , attackRange : ( Int, Int )
    , defRange : ( Int, Int )
    , powerOptions : List Power
    }


renderCity : Camera -> Float -> City -> Renderable
renderCity camera frame city =
    let
        ( x, y ) =
            toPoint city.position

        ( ox, oy ) =
            Types.getCameraOffsets camera

        chimmneyOffset =
            3 * sin (frame / 50)
    in
    shapes
        [ case city.owner of
            EnemyOwned ->
                fill Color.gray

            PlayerOwned ->
                fill Color.blue
        ]
        [ rect ( x + ox, y + oy ) 100 100
        , rect ( x + 40 + ox, y - 40 + oy + chimmneyOffset ) 20 80
        , rect ( x + 70 + ox, y - 100 + oy - chimmneyOffset ) 30 180
        ]
