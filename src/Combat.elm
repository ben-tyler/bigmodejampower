module Combat exposing (..)

import Cities exposing (City)
import Html exposing (Html, button, div, h1, h2, h3, h4, select, span, text)
import Html.Attributes exposing (disabled, selected, style, title)
import Html.Events exposing (onClick)
import Json.Decode exposing (maybe)
import Math.Vector2 exposing (Vec2)
import Player exposing (Player)
import Random
import Types exposing (Power, PowerEffect(..))


type alias GameDice =
    { id : Int
    , selected : Bool
    , dice : Dice
    }


findPowerForDice : List Dice -> Dice -> Int -> Int
findPowerForDice dices dice amt =
    dices
        |> List.filter (\d -> d == dice)
        |> List.map (\_ -> 1)
        |> List.sum
        |> (\s ->
                if s > 2 then
                    s

                else
                    0
           )


findAttachDeffenceAndPower : List Dice -> ( Int, Int, Int )
findAttachDeffenceAndPower dice =
    let
        pwr6 =
            findPowerForDice dice Six 6

        pwr5 =
            findPowerForDice dice Five 5

        pwr4 =
            findPowerForDice dice Four 4

        pwr3 =
            findPowerForDice dice Three 3

        attack =
            dice |> List.filter (\d -> d == Atck) |> List.map (\_ -> 1) |> List.sum

        deffence =
            dice |> List.filter (\d -> d == Def) |> List.map (\_ -> 1) |> List.sum
    in
    ( attack, deffence, pwr6 + pwr5 + pwr4 + pwr3 )


type Turn
    = PlayerTurn
    | EnemyTurn


type alias Model =
    { city : City
    , player : Player
    , dice : List GameDice
    , rollsLeft : Int
    , playerAtk : Int
    , playerDef : Int
    , playerPwr : Int
    , enemyAtck : Int
    , enemyDef : Int
    , turn : Turn
    , redText : String
    , enemyTurn : Maybe EnemyTurnData
    }


type Msg
    = Roll
    | NewRoll (List Dice)
    | SelectDice GameDice
    | Leave
    | CommitTurn
    | UsePower Power
    | GetTurnData EnemyTurnData


type Action
    = None
    | Exit
    | EnemyDefeted City
    | PlayerDefeted
    | DamageParticle Vec2 Int


getActionFromTurnResult : Model -> Action -> Action
getActionFromTurnResult nextModel action =
    if nextModel.enemyDef < 1 then
        EnemyDefeted nextModel.city

    else if nextModel.playerDef < 1 then
        PlayerDefeted

    else
        action


usePower : Power -> Model -> Model
usePower power combat =
    case power.effect of
        DestoryEnemyTurn ->
            { combat
                | playerPwr = combat.playerPwr - power.cost
                , enemyTurn =
                    case combat.enemyTurn of
                        Just turn ->
                            Just <| EnemyTurnData 0 0

                        Nothing ->
                            Nothing
            }

        IncreaseRolls ->
            { combat
                | playerPwr = combat.playerPwr - power.cost
                , rollsLeft = combat.rollsLeft + 3
            }

        BuffAttack ->
            { combat
                | playerPwr = combat.playerPwr - power.cost
                , playerAtk = combat.playerAtk + 2
            }

        BuffDefence ->
            { combat
                | playerPwr = combat.playerPwr - power.cost
                , playerDef = combat.playerDef + 2
            }

        DoubleDefence ->
            { combat
                | playerPwr = combat.playerPwr - power.cost
                , playerDef = combat.playerDef + combat.playerDef
            }

        DoubleAttack ->
            { combat
                | playerPwr = combat.playerPwr - power.cost
                , playerAtk = combat.playerAtk + combat.playerAtk
            }

        LowerDefence ->
            { combat
                | playerPwr = combat.playerPwr - power.cost
                , enemyDef = 1
            }

        NoEffect ->
            combat


removePower : Power -> Model -> Model
removePower power model =
    let
        player =
            model.player
    in
    { model
        | player =
            { player
                | powers =
                    player.powers
                        |> List.filter
                            (\p -> p.id /= power.id)
            }
    }


update : Msg -> Model -> ( Model, Cmd Msg, Action )
update msg model =
    case msg of
        GetTurnData data ->
            ( { model
                | enemyTurn = Just data
              }
            , Cmd.none
            , None
            )

        UsePower power ->
            if power.cost < model.playerPwr then
                ( usePower power model
                    |> removePower power
                , Cmd.none
                , None
                )

            else
                ( { model | redText = "Cant Afford Power" }
                , Cmd.none
                , None
                )

        CommitTurn ->
            case model.turn of
                PlayerTurn ->
                    let
                        ( atck, def, pwr ) =
                            model.dice |> List.map .dice |> findAttachDeffenceAndPower

                        damageToEnemy =
                            if atck > 0 then
                                DamageParticle model.city.position atck

                            else
                                None

                        nextModel =
                            { model
                                | enemyDef = model.enemyDef - (model.playerAtk + atck)
                                , playerPwr = model.playerPwr + pwr
                                , playerDef = model.playerDef + def
                                , turn = EnemyTurn
                                , dice = initDice
                                , rollsLeft = 5
                            }
                    in
                    ( nextModel
                    , Random.generate GetTurnData (enemyTurnGenerator model.city.attackRange model.city.defRange)
                    , getActionFromTurnResult nextModel damageToEnemy
                    )

                EnemyTurn ->
                    case model.enemyTurn of
                        Just data ->
                            let
                                nextModel =
                                    { model
                                        | enemyDef = model.enemyDef + data.defence
                                        , playerDef = model.playerDef - data.attack
                                        , turn = PlayerTurn
                                        , dice = initDice
                                        , rollsLeft = 5
                                    }
                            in
                            ( nextModel
                            , Cmd.none
                            , getActionFromTurnResult nextModel (DamageParticle model.player.position data.attack)
                            )

                        Nothing ->
                            ( model, Cmd.none, None )

        Leave ->
            ( model, Cmd.none, Exit )

        Roll ->
            if model.rollsLeft > 0 then
                ( { model | rollsLeft = model.rollsLeft - 1 }, Random.generate NewRoll rollGenerator, None )

            else
                ( model, Cmd.none, None )

        NewRoll newdices ->
            ( { model
                | dice =
                    List.map2
                        (\{ id, selected, dice } newDice ->
                            if selected then
                                { id = id, selected = False, dice = newDice }

                            else
                                { id = id, selected = False, dice = dice }
                        )
                        model.dice
                        newdices
              }
            , Cmd.none
            , None
            )

        SelectDice gameDice ->
            ( { model
                | dice =
                    model.dice
                        |> List.map
                            (\i ->
                                if i.id == gameDice.id then
                                    { i | selected = True }

                                else
                                    i
                            )
              }
            , Cmd.none
            , None
            )


type Dice
    = Atck
    | Def
    | Three
    | Four
    | Five
    | Six
    | NoDice


initDice : List GameDice
initDice =
    List.range 1 5
        |> List.map (\id -> { id = id, selected = True, dice = NoDice })


type alias EnemyTurnData =
    { attack : Int
    , defence : Int
    }


enemyTurnGenerator : ( Int, Int ) -> ( Int, Int ) -> Random.Generator EnemyTurnData
enemyTurnGenerator ( minAttack, maxAttacj ) ( minDef, maxDef ) =
    Random.map2
        EnemyTurnData
        (Random.int minAttack maxAttacj)
        (Random.int minDef maxDef)


rollGenerator : Random.Generator (List Dice)
rollGenerator =
    Random.uniform Atck
        [ Def
        , Three
        , Four
        , Five
        , Six
        ]
        |> Random.list 5


init : City -> Player -> Model
init city player =
    { city = city
    , player = player
    , dice = initDice
    , rollsLeft = 5
    , playerAtk = 0
    , playerDef = 5
    , playerPwr = 0
    , enemyAtck = 0
    , enemyDef = 5
    , turn = PlayerTurn
    , redText = ""
    , enemyTurn = Nothing
    }


view : Model -> Html Msg
view model =
    let
        ( atck, def, pwr ) =
            model.dice |> List.map .dice |> findAttachDeffenceAndPower
    in
    div
        [ style "position" "absolute"
        , style "top" "20px"
        , style "left" "175px"
        , style "width" "550px"
        , style "height" "330px"
        , style "font-family" "Audiowide"
        , style "background-color" "grey"

        -- , style "border" "3px solid #73AD21"
        ]
        [ div [ style "display" "flex" ]
            [ div [ style "margin" "10px", style "background-color" "coral", style "width" "120px" ]
                [ h3 [] [ text <| "Player " ]
                , h4 [] [ text <| "Atk " ++ String.fromInt (model.playerAtk + atck) ]
                , h4 [] [ text <| "Def " ++ String.fromInt model.playerDef ++ " (+" ++ String.fromInt def ++ ") " ]
                , h4 [] [ text <| "Pwr " ++ String.fromInt model.playerPwr ++ " (+" ++ String.fromInt pwr ++ ") " ]
                ]
            , div [ style "margin" "10px", style "text-align" "center", style "width" "300px" ]
                [ case model.turn of
                    PlayerTurn ->
                        let
                            selectedDice =
                                List.any (\i -> i.selected) model.dice |> not
                        in
                        div []
                            [ h1 [] [ text "Player Turn" ]
                            , button [ style "font-size" "20px", onClick Roll, disabled selectedDice ] [ text <| "Roll (" ++ String.fromInt model.rollsLeft ++ ")" ]
                            , model.dice
                                |> List.map
                                    (\dice ->
                                        button
                                            [ style "font-size" "40px"
                                            , onClick (SelectDice dice)
                                            , disabled dice.selected
                                            ]
                                            [ Html.text <| viewDice dice.dice ]
                                    )
                                |> div []
                            , div [] [ button [ style "font-size" "20px", onClick CommitTurn ] [ text <| "End Turn" ] ]
                            , div [] [ button [ style "font-size" "20px", onClick Leave ] [ text <| "Flee" ] ]
                            ]

                    EnemyTurn ->
                        div []
                            [ h1 [] [ text "Enemy Turn" ]
                            , case model.enemyTurn of
                                Just data ->
                                    div []
                                        [ h4 [] [ text <| "Attacking For 🗡" ++ String.fromInt data.attack ]
                                        , h4 [] [ text <| "Defending For 🛡" ++ String.fromInt data.defence ]
                                        , button [ style "font-size" "20px", onClick CommitTurn ] [ text <| "Next Turn" ]
                                        ]

                                Nothing ->
                                    div [] [ text "loading" ]
                            ]
                ]
            , div [ style "margin" "10px", style "background-color" "coral", style "width" "120px" ]
                [ h3 [] [ text "Enemy" ]
                , h4 [] [ text <| "Atk " ++ String.fromInt model.enemyAtck ]
                , h4 [] [ text <| "Def " ++ String.fromInt model.enemyDef ]
                ]
            ]
        , div []
            [ div [ style "margin-left" "10px" ]
                [ text "Powers "
                , model.player.powers
                    |> List.map
                        (\p ->
                            button
                                [ title p.description
                                , onClick (UsePower p)
                                ]
                                [ text <| p.title ++ "(" ++ String.fromInt p.cost ++ ")" ]
                        )
                    |> span []
                ]
            , h1 [ style "color" "red", style "text-align" "center" ]
                [ text model.redText ]
            ]
        ]


viewDice : Dice -> String
viewDice dice =
    case dice of
        Atck ->
            "🗡"

        --"⚀"
        Def ->
            "⛊"

        --"⚁"
        Three ->
            "⚂"

        Four ->
            "⚃"

        Five ->
            "⚄"

        Six ->
            "⚅"

        NoDice ->
            "◌"
