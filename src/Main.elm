module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onKeyUp)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Cities exposing (City)
import CollisionDetection exposing (BoundingBox, Collision(..))
import Color
import Combat
import Enemies
import Html exposing (Html, cite, div)
import Html.Attributes exposing (action, style)
import Html.Events
import Json.Decode as D
import Keys exposing (Keys, noKeys)
import Math.Vector2 as Vec2 exposing (Vec2, getX, vec2)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Particle exposing (Particle)
import Platform.Cmd as Cmd
import Player exposing (Player)
import PowerSelect
import Types exposing (Camera, toPoint)


type alias Dimensions =
    { width : Float, height : Float }


type CreatingPowerLine
    = Creating City


type alias PowerLine =
    { city1 : City
    , city2 : City
    }


type GameState
    = Playing
    | Defeated
    | Menu


type alias Model =
    { count : Float
    , width : Float
    , height : Float
    , player : Player
    , keys : Keys
    , cities : List City
    , combat : Maybe Combat.Model
    , camera : Camera
    , creatingPowerLine : Maybe CreatingPowerLine
    , powerLines : List PowerLine
    , powerSelect : Maybe PowerSelect.Model
    , particle : Particle.Model
    , enemies : Enemies.Model
    , gameState : GameState
    , dt : Float
    }


type Msg
    = Frame Float
    | KeyChanged Bool String
    | CombatMsg Combat.Msg
    | PowerSelectMsg PowerSelect.Msg
    | Test
    | ToMenu
    | StartGame


init : Dimensions -> ( Model, Cmd Msg )
init { width, height } =
    ( { width = width
      , height = height
      , count = 0
      , player = Player.init
      , keys = noKeys
      , cities = Cities.initCities
      , combat = Nothing
      , camera = Camera (vec2 50 300) (width / 2) (height / 2) (vec2 (width / 2) (height / 2))
      , creatingPowerLine = Nothing
      , powerLines = []
      , powerSelect = Nothing
      , particle = Particle.init |> Particle.create Nothing (vec2 100 100) Color.yellow
      , enemies = Enemies.init
      , gameState = Menu
      , dt = 0
      }
    , Cmd.none
    )


main : Program Dimensions Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


handleCombat : Combat.Msg -> Model -> ( Model, Cmd Msg )
handleCombat subMsg model =
    case model.combat of
        Just combat ->
            let
                ( submodel, subcmd, action ) =
                    Combat.update subMsg combat
            in
            case action of
                Combat.None ->
                    ( { model
                        | combat = Just submodel
                      }
                    , subcmd |> Cmd.map CombatMsg
                    )

                Combat.Exit ->
                    let
                        p =
                            model.player
                    in
                    ( { model
                        | combat = Nothing

                        -- , player = { p | position = vec2 50 50 }
                      }
                    , subcmd |> Cmd.map CombatMsg
                    )

                Combat.EnemyDefeted city ->
                    let
                        p =
                            model.player
                    in
                    ( { model
                        | combat = Nothing
                        , cities =
                            model.cities
                                |> List.map
                                    (\c ->
                                        if c.id == city.id then
                                            { c | owner = Cities.PlayerOwned }

                                        else
                                            c
                                    )
                        , particle = Particle.create (Just <| "Powerplant Obtained!") city.position Color.lightBlue model.particle

                        -- , player = { p | position = vec2 50 50 }
                      }
                    , subcmd |> Cmd.map CombatMsg
                    )

                Combat.PlayerDefeted ->
                    let
                        p =
                            model.player
                    in
                    ( { model
                        | combat = Nothing
                        , gameState = Defeated
                        , particle = Particle.create (Just <| "Death Blow") model.player.position Color.red model.particle
                      }
                    , subcmd |> Cmd.map CombatMsg
                    )

                Combat.DamageParticle pos amt ->
                    ( { model
                        | particle = Particle.create (Just <| String.fromInt amt) pos Color.red model.particle
                        , combat = Just submodel
                      }
                    , subcmd |> Cmd.map CombatMsg
                    )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            let
                ( m, c ) =
                    init { width = model.width, height = model.height }
            in
            ( { m | gameState = Playing }
            , c
            )

        ToMenu ->
            ( { model
                | gameState = Menu
              }
            , Cmd.none
            )

        Test ->
            ( { model
                | enemies = Enemies.addEnemy model.enemies model.cities
              }
            , Cmd.none
            )

        Frame dt ->
            ( { model
                | count = model.count + 1
                , particle = Particle.update dt model.particle
                , enemies = Enemies.update dt model.enemies
                , dt = dt
              }
                |> gameLoop dt
            , Cmd.none
            )

        KeyChanged isDown key ->
            ( { model | keys = Keys.updateKeys isDown key model.keys }
            , Cmd.none
            )

        CombatMsg submsg ->
            handleCombat submsg model

        PowerSelectMsg submsg ->
            case model.powerSelect of
                Just powerselect ->
                    let
                        ( psm, psc, action ) =
                            PowerSelect.update submsg powerselect
                    in
                    case action of
                        PowerSelect.MakeSelection power city ->
                            ( { model
                                | powerSelect = Nothing
                                , player = Player.addPower model.player power
                                , cities =
                                    model.cities
                                        |> List.map
                                            (\c ->
                                                if c.id == city.id then
                                                    { c | powerOptions = [] }

                                                else
                                                    c
                                            )
                              }
                            , psc |> Cmd.map PowerSelectMsg
                            )

                _ ->
                    ( model, Cmd.none )


gameLoop : Float -> Model -> Model
gameLoop dt model =
    let
        {- foo =
           model.cities
               |> List.map
                   (\city ->
                       CollisionDetection.checkCollision
                           { position = city.position, boundingBox = city.boundingBox }
                           { position = model.player.position, boundingBox = model.player.boundingBox }
                   )
               |> List.filter (\c -> c /= NoCollision)
        -}
        onCityPress =
            if model.keys.space then
                model.cities
                    |> List.filter (\c -> Vec2.distance (Vec2.add (vec2 50 50) c.position) model.player.position < 100)
                    |> List.head

            else
                Nothing

        camera =
            model.camera
    in
    case ( model.combat, model.powerSelect ) of
        ( Just combat, _ ) ->
            { model
                | player = Player.updatePlayer dt noKeys model.player
                , camera =
                    if Vec2.getY camera.position > Vec2.getY model.player.position - 200 then
                        { camera | position = Vec2.add (vec2 0 -1) model.camera.position }

                    else
                        model.camera
            }

        ( _, Just powerSelect ) ->
            { model
                | player = Player.updatePlayer dt noKeys model.player
                , camera =
                    if Vec2.getY camera.position > Vec2.getY model.player.position - 200 then
                        { camera | position = Vec2.add (vec2 0 -1) model.camera.position }

                    else
                        model.camera
            }

        ( Nothing, Nothing ) ->
            case onCityPress of
                Just city ->
                    handleSpaceBarClickOnCity city model

                Nothing ->
                    { model
                        | player = Player.updatePlayer dt model.keys model.player
                        , camera = { camera | position = model.player.position }
                        , creatingPowerLine =
                            if model.keys.space then
                                Nothing

                            else
                                model.creatingPowerLine
                    }


handleSpaceBarClickOnCity : City -> Model -> Model
handleSpaceBarClickOnCity city model =
    let
        powerLineConnected =
            case model.creatingPowerLine of
                Just (Creating creatingFromCity) ->
                    if creatingFromCity.id /= city.id && city.owner == Cities.PlayerOwned then
                        Just { city1 = city, city2 = creatingFromCity }

                    else
                        Nothing

                _ ->
                    Nothing
    in
    { model
        | combat =
            if city.owner == Cities.EnemyOwned then
                Just (Combat.init city model.player)

            else
                Nothing
        , creatingPowerLine =
            -- I have confused myself here
            case model.creatingPowerLine of
                Just (Creating creatingFromCity) ->
                    if creatingFromCity.id /= city.id && city.owner == Cities.PlayerOwned then
                        Nothing

                    else
                        model.creatingPowerLine

                Nothing ->
                    if city.owner == Cities.PlayerOwned then
                        Just <| Creating city

                    else
                        Nothing
        , powerLines =
            case powerLineConnected of
                Just mewpowerLine ->
                    mewpowerLine :: model.powerLines

                Nothing ->
                    model.powerLines
        , powerSelect =
            case powerLineConnected of
                Just newPowerLine ->
                    if newPowerLine.city2.powerOptions /= [] then
                        Just (PowerSelect.init newPowerLine.city2)

                    else if newPowerLine.city1.powerOptions /= [] then
                        Just (PowerSelect.init newPowerLine.city1)

                    else
                        Nothing

                Nothing ->
                    Nothing
        , particle =
            if city.owner == Cities.PlayerOwned && model.particle.effects == [] then
                Particle.create Nothing
                    city.position
                    Color.yellow
                    model.particle

            else
                model.particle
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        ]


view : Model -> Html Msg
view ({ width, height } as model) =
    div [ style "font-family" "Audiowide" ]
        [ --Html.text <| Debug.toString model.player.position
          case model.gameState of
            Playing ->
                viewPlaying model

            Menu ->
                viewMenu model

            Defeated ->
                viewDefeated model
        ]


viewDefeated : Model -> Html Msg
viewDefeated model =
    div
        []
        [ div
            [ style "position" "absolute"
            , style "top" "20px"
            , style "left" "0px"
            , style "width" "550px"
            , style "height" "330px"
            , style "text-align" "center"
            ]
            [ Html.h1 [] [ Html.text "YOU HAVE DIED" ]
            , Html.h1 [] [ Html.button [ style "font-family" "Audiowide", style "font-size" "20px", Html.Events.onClick ToMenu ] [ Html.text "fin" ] ]
            ]
        , Canvas.toHtml
            ( round model.width, round model.height )
            []
            [ clearScreen model
            , Player.renderPlayer model.count model.player model.camera
            , Particle.view model.camera model.particle
            ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    div
        []
        [ div
            [ style "position" "absolute"
            , style "top" "20px"
            , style "left" "0px"
            , style "width" "550px"
            , style "height" "330px"
            , style "text-align" "center"
            , style "font-size" "12px"
            ]
            [ Html.h1 [] [ Html.text "Power Plant Dice" ]
            , Html.h3 [] [ Html.text "How to play: " ]
            , Html.p []
                [ Html.text "Move with arrows or wasd" ]
            , Html.p []
                [ Html.text "Press Space to interact with power plants -> Grey Power Plants (Enemy Owned) will start dice game. Blue Power Plants (Player Owned) will start power connection" ]
            , Html.h3 [] [ Html.text "Dice Game " ]
            , Html.p []
                [ Html.text "Roll the dice, then select the dice you would like to re-roll. Sword Icons will do +1 Damage this turn, Sheild Icons will add +1 to Defence" ]
            , Html.p []
                [ Html.text "Two or more of a dice kind will add power, which can be spent on powers the you have collected" ]
            , Html.p []
                [ Html.text "After winning the match you will take controll of the power plant" ]
            , Html.h3 [] [ Html.text "Connect Power Plants " ]
            , Html.p []
                [ Html.text "Connect two power plants you own to select a power you can use in the dice game" ]
            , Html.div [] [ Html.button [ style "font-family" "Audiowide", style "font-size" "20px", Html.Events.onClick StartGame ] [ Html.text "Start Game" ] ]
            ]
        ]


viewPlaying : Model -> Html Msg
viewPlaying ({ width, height } as model) =
    let
        buffer =
            50

        -- Adjust this value to control how early cities go behind
        citiesInFrontOfPlayer =
            model.cities
                |> List.filter
                    (\i ->
                        Vec2.getY i.position < Vec2.getY model.player.position - buffer
                    )

        citiesBehindPlayer =
            model.cities
                |> List.filter
                    (\i ->
                        Vec2.getY i.position >= Vec2.getY model.player.position - buffer
                    )
    in
    div []
        [ -- Html.button [ Html.Events.onClick Test ] [ Html.text "test" ]
          case ( model.combat, model.powerSelect ) of
            ( Just combat, Nothing ) ->
                Combat.view combat |> Html.map CombatMsg

            ( Nothing, Just powerSelect ) ->
                PowerSelect.view powerSelect |> Html.map PowerSelectMsg

            ( Nothing, Nothing ) ->
                div [] []

            _ ->
                div [] [ Html.text "ERROR" ]
        , Canvas.toHtml
            ( round width, round height )
            []
            ([ clearScreen model
             , background model.camera
             ]
                ++ List.map (Cities.renderCity model.camera model.count) citiesInFrontOfPlayer
                ++ [ Player.renderPlayer model.count model.player model.camera ]
                ++ List.map (Cities.renderCity model.camera model.count) citiesBehindPlayer
                ++ [ Particle.view model.camera model.particle ]
                ++ List.map (renderPowerLine model.camera) model.powerLines
                ++ [ powerLine model.camera model.player model.creatingPowerLine
                   , Enemies.view model.camera model.enemies
                   ]
            )
        ]


renderPowerLine : Camera -> PowerLine -> Renderable
renderPowerLine camera powerline =
    let
        ( ox, oy ) =
            Types.getCameraOffsets camera

        ( px, py ) =
            toPoint powerline.city1.position

        ( cx, cy ) =
            toPoint powerline.city2.position
    in
    shapes
        [ stroke Color.yellow
        , lineWidth 5
        ]
        [ path ( px + ox, py + oy )
            [ lineTo ( cx + ox + 10, cy + oy + 10 )
            ]
        ]


clearScreen : Model -> Renderable
clearScreen { width, height } =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


powerLine : Camera -> Player -> Maybe CreatingPowerLine -> Renderable
powerLine camera player maybeCreating =
    case maybeCreating of
        Just (Creating city) ->
            let
                ( ox, oy ) =
                    Types.getCameraOffsets camera

                ( px, py ) =
                    toPoint player.position

                ( cx, cy ) =
                    toPoint city.position
            in
            shapes
                [ stroke Color.yellow
                , lineWidth 5
                ]
                [ path ( px + ox, py + oy )
                    [ lineTo ( cx + ox + 10, cy + oy + 10 )
                    ]
                ]

        Nothing ->
            shapes [] []


background : Camera -> Canvas.Renderable
background camera =
    let
        ( ox, oy ) =
            Types.getCameraOffsets camera

        gridLines =
            []

        treeDecorations =
            [ circle ( -100 + ox, 150 + oy ) 30
            , circle ( 50 + ox, 300 + oy ) 30
            , circle ( 200 + ox, 200 + oy ) 30
            ]

        waterPatches =
            [ rect ( -50 + ox, 450 + oy ) 100 50
            , rect ( 150 + ox, 400 + oy ) 80 30
            ]
    in
    group
        []
        [ shapes
            [ fill Color.darkGreen ]
            [ rect ( -1000 + ox, -500 + oy ) 3000 3000 ]
        , shapes
            [ stroke Color.black ]
            gridLines
        , shapes
            [ fill Color.green ]
            []
        , shapes
            [ fill Color.gray ]
            []
        ]
