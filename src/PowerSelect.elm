module PowerSelect exposing (..)

import Cities exposing (City)
import Html exposing (Html, button, div, h1, h2, h3, h4, select, text)
import Html.Attributes exposing (disabled, selected, style)
import Html.Events exposing (onClick)
import Types exposing (Power, PowerEffect(..))


type alias Model =
    { city : City
    , powerOptions : List Power
    }


init : City -> Model
init city =
    { city = city
    , powerOptions = city.powerOptions
    }


type Msg
    = PowerSelect Power


type Action
    = MakeSelection Power Cities.City


update : Msg -> Model -> ( Model, Cmd Msg, Action )
update msg model =
    case msg of
        PowerSelect power ->
            ( model, Cmd.none, MakeSelection power model.city )


view : Model -> Html Msg
view model =
    div
        [ style "position" "absolute"
        , style "top" "20px"
        , style "left" "300px"
        , style "width" "300px"
        , style "height" "300px"
        , style "font-family" "Audiowide"
        , style "background-color" "grey"
        , style "text-align" "center"

        -- , style "border" "3px solid #73AD21"
        ]
        [ h2 [] [ text "Select Power" ]
        , model.powerOptions
            |> List.map
                (\po ->
                    div []
                        [ button
                            [ onClick (PowerSelect po) ]
                            [ h4 [] [ text <| po.title ++ "  (" ++ String.fromInt po.cost ++ ")" ]
                            , div [] [ text po.description ]
                            ]
                        ]
                )
            |> div []
        ]
