module Main exposing (..)

import AnimationFrame
import Html exposing (..)
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Canvas.Point exposing (Point, fromInts)
import Canvas.Events as Events
import Task
import Time exposing (Time)


main : Program Never Model Msg
main =
    Html.program
        { init = ( Loading, loadImage )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


-- TYPES
type Msg
    = ImageLoaded (Result Error Canvas)
    | Viewport Point
    -- | Delta Time

type Model
    = GotCanvas Canvas (List DrawOp)
    | Loading



loadImage : Cmd Msg
loadImage =
    Task.attempt
        ImageLoaded
        (Canvas.loadImage "./C10-LRGB-B1-70perc.jpg")


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ImageLoaded result ->
            case Result.toMaybe result of
                Just canvas ->
                    ( GotCanvas canvas []
                    , Cmd.none
                    )
                Nothing ->
                    ( Loading
                    , loadImage
                    )

        Viewport point ->
            case model of
                Loading ->
                    ( Loading
                    , loadImage
                    )
                GotCanvas canvas drawOps ->
                    ( GotCanvas canvas (viewport point canvas)
                    , Cmd.none
                    )
        -- Delta dt ->
        --     (model, Cmd.none)

viewport : Point -> Canvas -> List DrawOp
viewport  point canvas =
        [DrawImage canvas (CropScaled point (Size 1000 750) (fromInts (0,0)) (Size 1000 750))]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
          -- Window.resizes Resize
          --  AnimationFrame.diffs Delta
          -- , Keyboard.downs KeyDown
        ]


-- VIEW
view : Model -> Html Msg
view model =
    div
        []
        [ presentIfReady model ]

presentIfReady : Model -> Html Msg
presentIfReady model =
    case model of
        Loading ->
            p [] [ text "Loading image" ]

        GotCanvas canvas drawOps ->
            Canvas.initialize (Size 1000 750)
                |> Canvas.batch drawOps
                |> Canvas.toHtml [ Events.onMouseMove Viewport ]
                |> List.singleton
                |> div []
