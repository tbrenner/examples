module Main exposing (..)

import AnimationFrame
import Canvas exposing (Canvas, Error, DrawOp, DrawOp(..), DrawImageParams(..))
import Canvas.Point exposing (Point, fromInts, fromFloats)
import Html exposing (..)
import Key exposing (..)
import Keyboard exposing (KeyCode)
import Time exposing (Time)
import Task
import Window exposing (Size)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.batch [ initialSize, loadImage ] )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- TYPES


type alias Coordinates =
    { x : Float, y : Float }


type alias Velocity =
    { horz : Float, vert : Float }


type Axis
    = Horizontal
    | Vertical


type Msg
    = SizeUpdated Size
    | ImageLoaded (Result Error Canvas)
    | KeyDown KeyCode
    | TimeUpdate Time


type alias Model =
    { canvas : Canvas
    , imgSize : Size
    , imgClipOffset : Coordinates
    , windowSize : Size
    , player : Coordinates
    , velocity : Velocity
    }


initModel : Model
initModel =
    { canvas = Canvas.initialize { width = 0, height = 0 }
    , imgSize = Size 2797 1870
    , imgClipOffset = Coordinates 0 0
    , windowSize = Size 0 0
    , player = Coordinates 1398 935
    , velocity = Velocity 0 0
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes SizeUpdated
        , Keyboard.downs KeyDown

        -- , AnimationFrame.diffs TimeUpdate
        ]



-- INITIALIZE


initialSize : Cmd Msg
initialSize =
    Window.size
        |> Task.perform SizeUpdated


loadImage : Cmd Msg
loadImage =
    Task.attempt
        ImageLoaded
        (Canvas.loadImage "./C10-LRGB-B1-70perc.jpg")



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SizeUpdated newsize ->
            ( { model | windowSize = newsize }, Cmd.none )

        ImageLoaded result ->
            case Result.toMaybe result of
                Just canvas ->
                    ( { model | canvas = canvas }, Cmd.none )

                Nothing ->
                    ( model, loadImage )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        TimeUpdate dt ->
            ( applyPhysics dt model, Cmd.none )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        ArrowUp ->
            updateVelocity Vertical -0.25 model

        ArrowDown ->
            updateVelocity Vertical 0.25 model

        ArrowLeft ->
            updateVelocity Horizontal -0.25 model

        ArrowRight ->
            updateVelocity Horizontal 0.25 model

        OtherKey ->
            model


updateVelocity : Axis -> Float -> Model -> Model
updateVelocity axis newVelocity model =
    case axis of
        Vertical ->
            { model | velocity = Velocity 0 newVelocity }

        Horizontal ->
            { model | velocity = Velocity newVelocity 0 }


applyPhysics : Float -> Model -> Model
applyPhysics dt model =
    let
        maxClipOffsetX =
            model.imgSize.width - model.windowSize.width |> toFloat

        updatedClipOffsetX =
            model.imgClipOffset.x + model.velocity.horz * dt

        x_ =
            clamp 0 maxClipOffsetX updatedClipOffsetX

        maxClipOffsetY =
            model.imgSize.height - model.windowSize.height |> toFloat

        updatedClipOffsetY =
            model.imgClipOffset.y + model.velocity.vert * dt

        y_ =
            clamp 0 maxClipOffsetY updatedClipOffsetY
    in
        { model | imgClipOffset = Coordinates x_ y_ }



-- VIEW


view : Model -> Html Msg
view model =
    Canvas.toHtml [] (viewport model)


viewport : Model -> Canvas
viewport model =
    let
        point =
            fromFloats ( model.imgClipOffset.x, model.imgClipOffset.y )

        drawImageParams =
            CropScaled point model.windowSize (fromInts ( 0, 0 )) model.windowSize

        drawOps =
            [ DrawImage model.canvas drawImageParams ]
    in
        Canvas.initialize model.windowSize
            |> Canvas.batch drawOps
