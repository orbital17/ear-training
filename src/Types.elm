module Types exposing (..)

import Music exposing (Chord, Note)
import Set exposing (Set)
import Utils


type Msg
    = NoOp
    | Play (List Chord)
    | PlayCustomDelay Float (List Chord)
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
    , autoProceed = False
    , chordSize = 3
    , chordsInSequence = 1
    , delay = 1
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


questionToString : Question -> String
questionToString q =
    case q.qType of
        Degree ->
            Music.syllable q.answer

        IntervalName ->
            Maybe.withDefault "" (Utils.get q.answer Music.intervalNames)


getOptionsFromModel : Model -> List AnswerOption
getOptionsFromModel m =
    m.currentQuestion
        |> Maybe.map .qType
        |> Maybe.map (getOptions m.settings.mode)
        |> Maybe.withDefault []


getQuestion : Model -> Maybe Question
getQuestion m =
    let
        name chord =
            case chord of
                [ a, b ] ->
                    { qType = IntervalName, answer = b - a }

                _ ->
                    { qType = IntervalName, answer = 0 }

        degree n =
            { qType = Degree, answer = (n % 12) }

        currentChord =
            Utils.get m.guessed.chords m.chordsToGuess

        question chord =
            case (Utils.get m.guessed.notes chord) of
                Just note ->
                    degree note

                Nothing ->
                    name chord
    in
        currentChord
            |> Maybe.map question
            |> (\q ->
                    if q == Nothing then
                        m.currentQuestion
                    else
                        q
               )


nextQuestion : Model -> Model
nextQuestion m =
    let
        currentChordSize =
            Utils.get m.guessed.chords m.chordsToGuess
                |> Maybe.withDefault []
                |> List.length

        g =
            m.guessed

        newGuessed =
            if g.notes < currentChordSize - 1 then
                { g | notes = g.notes + 1 }
            else if m.settings.guessChordName && g.notes == currentChordSize - 1 then
                { g | notes = g.notes + 1 }
            else
                { g | notes = 0, chords = g.chords + 1 }

        newModel =
            { m | guessed = newGuessed, attemps = Set.empty }
    in
        { newModel | currentQuestion = getQuestion newModel }


type alias Model =
    { stat : Statistics
    , settings : Settings
    , chordsToGuess : List Chord
    , currentQuestion : Maybe Question
    , error : Bool
    , guessed : { chords : Int, notes : Int }
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
    , currentQuestion = Nothing
    , error = False
    , guessed = { chords = 0, notes = 0 }
    , attemps = Set.empty
    , page = MainPage
    }


allGuessed : Model -> Bool
allGuessed model =
    model.guessed.chords == List.length model.chordsToGuess
