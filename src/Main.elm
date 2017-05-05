port module Main exposing (main)


import Html exposing (li, div, Html, text, button)
--import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Chords exposing (Chord, randomNote)
import Random

main : Program Never Model Msg
main =
    Html.program
        { init = (emptyModel ! [])
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

-- MODEL

type alias Model = { }

emptyModel : Model
emptyModel = {}

port playChord: Chord -> Cmd msg
port stop: () -> Cmd msg


-- UPDATE

playMaybeNote : Maybe Chords.Note -> Msg
playMaybeNote maybe =
    case maybe of
        Just n -> Play [n]
        _ -> NoOp

type Msg = Play Chord | Stop | RandomNote | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> model ! []
        Play chord ->
            model ! [ playChord chord ]
        Stop ->
            ( model, stop () )
        RandomNote ->
            ( model, Random.generate playMaybeNote <| randomNote Chords.Major )

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text "Hello world"
    , button [ onClick (Play [60, 72])] [text "tonic"]
    , button [ onClick (Stop)] [text "stop"]
    , button [ onClick (RandomNote)] [text "random"]
    ]