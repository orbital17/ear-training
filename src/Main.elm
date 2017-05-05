port module Main exposing (main)


import Html exposing (li, div, Html, text, button)
--import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Char
import Random
import Keyboard

import Chords exposing (Chord, getRandom)

main : Program Never Model Msg
main =
    Html.program
        { init = (initModel ! [])
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model = 
    { root: Int
    , chord: Chord
    }

initModel : Model
initModel = 
    { root = 48
    , chord = []
    }

port playChord: Chord -> Cmd msg
port stop: () -> Cmd msg

play : Int -> Chord -> Cmd msg
play root c = Chords.transpose root c |> playChord
-- UPDATE

type Msg = NoOp | Play Chord | Stop | NewExercise | Exercise Chord 
    | KeyPress Char

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> model ! []
        Play chord ->
            model ! [ play model.root chord  ]
        Stop ->
            ( model, stop () )
        NewExercise ->
            ( model, Random.generate Exercise getRandom )
        Exercise c ->
            ({ model | chord = c }, play model.root c)
        KeyPress k ->
            k 
                |> Debug.log "keypress"
                |> \_ -> model ! []

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text "Ear training"
    , button [ onClick (Play [0, 12])] [text "tonic"]
    , button [ onClick (NewExercise)] [text "next"]
    , button [ onClick (Play model.chord)] [text "hear"]
    ]

subscriptions : Model -> Sub Msg
subscriptions m =
    Keyboard.presses <| KeyPress << Char.fromCode