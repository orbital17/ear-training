module Types exposing (..)

import Chords exposing (Chord, Note)
import Set exposing (Set)


type Msg
    = NoOp
    | Play Chord
    | NewExercise
    | Exercise Chord
    | MakeGuess Note
    | StartExercises


type alias Model =
    { root : Int
    , chord : Chord
    , total : Int
    , correct : Int
    , error : Bool
    , guessed : Int
    , attemps : Set Note
    , mode : Chords.Mode
    , page : Page
    }


type Page
    = MainPage
    | ExercisePage


initModel : Model
initModel =
    { root = 48
    , chord = []
    , total = 0
    , correct = 0
    , error = False
    , guessed = 0
    , attemps = Set.empty
    , mode = Chords.Major
    , page = MainPage
    }


allGuessed : Model -> Bool
allGuessed model =
    model.guessed == List.length model.chord
