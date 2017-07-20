module Main exposing (..)

-- import AnimationFrame

import Canvas exposing (Canvas, DrawImageParams(..), DrawOp, DrawOp(..), Error, Size)
import Canvas.Events as Events
import Canvas.Point exposing (Point, fromInts)
import Html exposing (..)
import Task
import Window exposing (Size)


-- import Time exposing (Time)


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( Loading, loadImage )
            -- { init = ( Loading, loadSize )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- TYPES


type Msg
    = ImageLoaded (Result Error Canvas)
    | InitSize Window.Size
    | Viewport Point



-- | Delta Time
--
-- type Model
--     = GotCanvas Canvas (List DrawOp) Canvas.Size
--     | Loading


type State
    = Loading
    | Loaded


type alias Model =
    { state : State
    , canvas : Canvas
    , ops : List DrawOp
    , canvas_size : Canvas.Size
    }


model : Model
model =
    { state = Loading
    , canvas = Canvas.initialize 0 0
    , ops = []
    , canvas_size = Canvas.Size 0 0
    }


loadImage : Cmd Msg
loadImage =
    Task.attempt
        ImageLoaded
        (Canvas.loadImage "./C10-LRGB-B1-70perc.jpg")


loadSize : Cmd Msg
loadSize =
    Task.perform InitSize (Debug.log "window size:" Window.size)


initSize : Canvas.Size
initSize =
    { width = 700, height = 500 }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ImageLoaded result ->
            case Result.toMaybe result of
                Just canvas ->
                    ( GotCanvas canvas [] initSize
                    , Cmd.none
                    )

                Nothing ->
                    ( Loading
                    , loadImage
                    )

        InitSize size ->
            ( Debug.log "InitSize model" model, Cmd.none )

        Viewport point ->
            case model of
                Loading ->
                    ( Loading
                    , loadImage
                    )

                GotCanvas canvas drawOps size ->
                    ( GotCanvas canvas (viewport point size canvas) size
                    , Cmd.none
                    )



-- Delta dt ->
--     (model, Cmd.none)


viewport : Point -> Canvas.Size -> Canvas -> List DrawOp
viewport point size canvas =
    [ DrawImage canvas (CropScaled point size (fromInts ( 0, 0 )) size) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [-- Window.resizes Resize
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

        GotCanvas canvas drawOps size ->
            Canvas.initialize (Canvas.Size 1000 750)
                |> Canvas.batch drawOps
                |> Canvas.toHtml [ Events.onMouseMove Viewport ]
                |> List.singleton
                |> div []
