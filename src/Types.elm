module Types exposing (..)

import Music exposing (Chord, Note)
import Set exposing (Set)
import Utils


type Msg
    = NoOp
    | Play (List Chord)
    | NewExercise
    | Exercise (List Chord)
    | MakeGuess Note
    | StartExercises
    | ChangeSettings (Settings -> Settings)
    | MoveToPage Page


type alias Settings =
    { root : Int
    , mode : Music.Mode
    , guessChordName : Bool
    , autoProceed : Bool
    , chordSize : Int
    , chordsInSequence : Int
    , delay : Float
    }


initSettings : Settings
initSettings =
    { root = 48
    , mode = Music.Major
    , guessChordName = False
    , autoProceed = True
    , chordSize = 2
    , chordsInSequence = 2
    , delay = 0.5
    }


type alias Statistics =
    { total : Int
    , correct : Int
    }


initStatistics : Statistics
initStatistics =
    { total = 0, correct = 0 }


type QuestionType
    = Degree
    | IntervalName


type alias Question =
    { qType : QuestionType
    , answer : Int
    }


type alias AnswerOption =
    { name : String
    , index : Int
    , keyMap : Char
    }


gammaKeyMap : List Char
gammaKeyMap =
    [ '1', 'w', '2', 'e', '3', '4', 'r', '5', 'y', '6', 'u', '7', '8' ]


getOptions : Music.Mode -> QuestionType -> List AnswerOption
getOptions m t =
    case t of
        Degree ->
            Music.modeNotes m
                |> List.map
                    (\n ->
                        { name = Music.syllable n
                        , index = n
                        , keyMap = Maybe.withDefault 'd' (Utils.get n gammaKeyMap)
                        }
                    )

        IntervalName ->
            let
                answerOption name index keyMap =
                    { name = name, index = index, keyMap = keyMap }
            in
                List.map3 answerOption Music.intervalNames (List.range 0 12) gammaKeyMap


getOptionsFromModel : Model -> List AnswerOption
getOptionsFromModel m =
    let
        get n =
            Utils.get n m.questions
                |> Maybe.map .qType
                |> Maybe.map (getOptions m.settings.mode)

        current =
            get m.guessed
    in
        case current of
            Just _ ->
                current |> Maybe.withDefault []

            Nothing ->
                get (m.guessed - 1) |> Maybe.withDefault []


getQuestions : Bool -> List Chord -> List Question
getQuestions guessChordName chords =
    let
        name chord =
            case chord of
                [ a, b ] ->
                    { qType = IntervalName, answer = b - a }

                _ ->
                    { qType = IntervalName, answer = 0 }

        fromChord chord =
            List.map (\n -> { qType = Degree, answer = (n % 12) }) chord
                ++ if guessChordName then
                    [ name chord ]
                   else
                    []
    in
        List.map fromChord chords
            |> List.concat


type alias Model =
    { stat : Statistics
    , settings : Settings
    , chordsToGuess : List Chord
    , questions : List Question
    , error : Bool
    , guessed : Int
    , attemps : Set Int
    , page : Page
    }


type Page
    = MainPage
    | ExercisePage
    | SettingsPage


initModel : Model
initModel =
    { stat = initStatistics
    , settings = initSettings
    , chordsToGuess = []
    , questions = []
    , error = False
    , guessed = 0
    , attemps = Set.empty
    , page = MainPage
    }


allGuessed : Model -> Bool
allGuessed model =
    model.guessed == List.length model.questions
