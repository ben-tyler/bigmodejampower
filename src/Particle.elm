module Particle exposing (..)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Text exposing (font)
import Color
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Types exposing (Camera, getPosWithCamera, toPoint)


type alias Particle =
    { position : Vec2
    , velocity : Vec2
    , lifetime : Float
    }


type alias ParticleEffect =
    { particles : List Particle
    , text : Maybe String
    , color : Color.Color
    , position : Vec2
    }


type alias Model =
    { effects : List ParticleEffect
    }


init : Model
init =
    { effects = []
    }


create : Maybe String -> Vec2 -> Model -> Model
create maybeString position model =
    let
        newEffect =
            { particles =
                [ { position = Vec2.add position (vec2 1 1), velocity = Vec2.vec2 1 -1, lifetime = 250 }
                , { position = Vec2.add position (vec2 2 -1), velocity = Vec2.vec2 -1 -1, lifetime = 250 }
                , { position = Vec2.add position (vec2 -1 2), velocity = Vec2.vec2 1 0, lifetime = 250 }
                , { position = Vec2.add position (vec2 -2 -2), velocity = Vec2.vec2 -1 1, lifetime = 250 }
                , { position = Vec2.add position (vec2 0 3), velocity = Vec2.vec2 1 -0.5, lifetime = 150 }
                , { position = Vec2.add position (vec2 -3 0), velocity = Vec2.vec2 -0.5 0.5, lifetime = 150 }
                , { position = Vec2.add position (vec2 2 2), velocity = Vec2.vec2 0 1, lifetime = 150 }
                , { position = Vec2.add position (vec2 -1 -3), velocity = Vec2.vec2 1 0.5, lifetime = 150 }
                , { position = Vec2.add position (vec2 3 -2), velocity = Vec2.vec2 -1 -0.5, lifetime = 150 }
                , { position = Vec2.add position (vec2 -2 1), velocity = Vec2.vec2 0 -1, lifetime = 150 }
                ]
            , text = maybeString
            , color = Color.red
            , position = position
            }
    in
    { model | effects = newEffect :: model.effects }


update : Float -> Model -> Model
update deltaTime model =
    let
        updateParticle p =
            { p
                | position = Vec2.add p.position p.velocity
                , lifetime = p.lifetime - 1
            }

        updateEffect effect =
            { effect
                | particles =
                    List.map updateParticle effect.particles
                        |> List.filter (\p -> p.lifetime > 0)
            }
    in
    { model
        | effects =
            List.map updateEffect model.effects
                |> List.filter (\e -> e.particles /= [])
    }


view : Camera -> Model -> Renderable
view camera model =
    model.effects
        |> List.concatMap
            (\effect ->
                (case effect.text of
                    Just t ->
                        text [ font { size = 40, family = "serif" }, fill effect.color ] (getPosWithCamera effect.position camera) t

                    Nothing ->
                        shapes [] []
                )
                    :: List.map
                        (\p ->
                            shapes [ fill effect.color ]
                                [ circle (getPosWithCamera p.position camera) 3 ]
                        )
                        effect.particles
            )
        |> group []
