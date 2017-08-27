module Main exposing (..)

import Html exposing (..)
import Canvas exposing (Point, Size, Error, DrawOp(..), Canvas)
import Task


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
    | Move Point


type Model
    = GotCanvas Canvas
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
                    ( GotCanvas canvas, Cmd.none )

                Nothing ->
                    ( Loading, loadImage )

        Move position ->
            case model of
                Loading ->
                    ( Loading, loadImage )

                GotCanvas canvas ->
                    ( GotCanvas canvas, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ p [] [ text "Elm-Canvas" ]
        , presentIfReady model
        ]


presentIfReady : Model -> Html Msg
presentIfReady model =
    case model of
        Loading ->
            p [] [ text "Loading image" ]

        GotCanvas canvas ->
            Canvas.toHtml [] canvas
