module Keys exposing (..)


type alias Keys =
    { up : Bool
    , left : Bool
    , down : Bool
    , right : Bool
    , space : Bool
    }


noKeys : Keys
noKeys =
    Keys False False False False False


updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
    case key of
        " " ->
            { keys | space = isDown }

        "ArrowUp" ->
            { keys | up = isDown }

        "ArrowLeft" ->
            { keys | left = isDown }

        "ArrowDown" ->
            { keys | down = isDown }

        "ArrowRight" ->
            { keys | right = isDown }

        "w" ->
            { keys | up = isDown }

        "a" ->
            { keys | left = isDown }

        "s" ->
            { keys | down = isDown }

        "d" ->
            { keys | right = isDown }

        _ ->
            keys
