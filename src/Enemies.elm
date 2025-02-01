module Enemies exposing (..)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Cities exposing (City)
import Color
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Types exposing (Camera, getPosWithCamera)


type alias Model =
    { enemies : List Enemy
    }


type alias Enemy =
    { position : Vec2
    , destiniation : Vec2
    , attackRange : ( Int, Int )
    , defRange : ( Int, Int )
    }


init : Model
init =
    { enemies = []
    }


addEnemy : Model -> List City -> Model
addEnemy model cities =
    let
        selectedDestination =
            cities
                |> List.filter (\c -> c.owner == Cities.PlayerOwned)
                |> List.head
                |> Maybe.andThen (\c -> Just c.position)
                |> Maybe.withDefault (vec2 0 0)
    in
    { model
        | enemies =
            { position = vec2 500 500
            , destiniation = selectedDestination
            , attackRange = ( 1, 5 )
            , defRange = ( 1, 5 )
            }
                :: model.enemies
    }


update : Float -> Model -> Model
update deltaTime model =
    { model
        | enemies =
            model.enemies
                |> List.map
                    (\e ->
                        let
                            direction =
                                Vec2.direction e.destiniation e.position
                                    |> Vec2.scale 0.1
                        in
                        { e
                            | position = Vec2.add e.position direction
                        }
                    )
    }


view : Camera -> Model -> Renderable
view camera model =
    model.enemies
        |> List.map
            (\e ->
                shapes
                    [ fill Color.charcoal ]
                    [ circle (getPosWithCamera e.position camera) 20 ]
            )
        |> group []
