port module Main exposing (main)


import Html exposing (li, div, Html, text, button)
--import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Chords exposing (Chord, getRandom)
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

type Msg = Play Chord | Stop | RandomChord | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> model ! []
        Play chord ->
            model ! [ playChord chord ]
        Stop ->
            ( model, stop () )
        RandomChord ->
            let playMaybeChord maybe =
                case maybe of
                    Just c -> Play c
                    _ -> NoOp
            in
            ( model, Random.generate playMaybeChord <| getRandom )

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text "Ear training"
    , button [ onClick (Play [60, 72])] [text "tonic"]
    , button [ onClick (Stop)] [text "stop"]
    , button [ onClick (RandomChord)] [text "random"]
    ]