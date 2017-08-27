module Key exposing (..)


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | OtherKey


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        37 ->
            ArrowLeft

        39 ->
            ArrowRight

        38 ->
            ArrowUp

        40 ->
            ArrowDown

        _ ->
            OtherKey
