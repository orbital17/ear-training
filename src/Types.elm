module Types exposing (..)

import Chords exposing (Chord, Note)
import Set exposing (Set)


type Msg
    = NoOp
    | PlayOne Chord
    | Play (List Chord)
    | NewExercise
    | Exercise Chord
    | MakeGuess Note
    | StartExercises


type alias Settings =
    { root : Int
    , mode : Chords.Mode
    }


initSettings : Settings
initSettings =
    { root = 48
    , mode = Chords.Major
    }


type alias Statistics =
    { total : Int
    , correct : Int
    }


initStatistics : Statistics
initStatistics =
    { total = 0, correct = 0 }


type alias Model =
    { stat : Statistics
    , settings : Settings
    , chord : Chord
    , error : Bool
    , guessed : Int
    , attemps : Set Note
    , page : Page
    }


type Page
    = MainPage
    | ExercisePage


initModel : Model
initModel =
    { stat = initStatistics
    , settings = initSettings
    , chord = []
    , error = False
    , guessed = 0
    , attemps = Set.empty
    , page = MainPage
    }


allGuessed : Model -> Bool
allGuessed model =
    model.guessed == List.length model.chord
