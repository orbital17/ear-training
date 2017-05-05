port module Main exposing (main)


import Html exposing (li, div, Html, text, button)
--import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Chords exposing (Chord)

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

type Msg = Play Chord | Stop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Play chord ->
            model ! [ playChord chord ]
        Stop ->
            ( model, stop () )

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text "Hello world"
    , button [ onClick (Play [60, 64])] [text "play"]
    , button [ onClick (Stop)] [text "stop"]
    ]