port module Main exposing (main)

import Char
import Html
import Keyboard
import Random
import Set exposing (Set)
import Music exposing (Chord, Note)
import Types exposing (Msg(..), Model, Page(..), allGuessed, initModel, Statistics)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        --{ init = (update StartExercises initModel)
        --{ init = ({ initModel | page = SettingsPage } ! [])
        { init = (initModel ! [])
        , view = view
        , update = debugUpdate
        , subscriptions = subscriptions
        }


port playChordsPort : ( List Chord, Float ) -> Cmd msg


play : Int -> Float -> List Chord -> Cmd msg
play root timeInterval chords =
    playChordsPort ( List.map (Music.transpose root) chords, timeInterval )


countExercise : Model -> Statistics
countExercise model =
    let
        stat =
            model.stat
    in
        { total = stat.total + 1
        , correct =
            if model.error then
                stat.correct
            else
                stat.correct + 1
        }


debugUpdate : Msg -> Model -> ( Model, Cmd Msg )
debugUpdate msg model =
    let
        ( newModel, cmds ) =
            update msg model

        _ =
            Debug.log "msg" msg

        _ =
            Debug.log "model" newModel
    in
        ( newModel, cmds )


getRandomChords : Types.Settings -> Random.Generator (List Chord)
getRandomChords s =
    let
        transpose octave chord =
            Music.transpose (12 * octave) chord

        getChord =
            Random.map2 transpose (Random.int 0 1) (Music.getRandom s.mode s.chordSize)
    in
        Random.list s.chordsInSequence getChord


getNewExercise : Model -> ( Model, Cmd Msg )
getNewExercise model =
    ( { model | guessed = { chords = 0, notes = 0 }, error = False }
    , Random.generate Exercise (getRandomChords model.settings)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Play chords ->
            model ! [ play model.settings.root model.settings.delay chords ]

        NewExercise ->
            if allGuessed model then
                getNewExercise model
            else
                (model ! [])

        MakeGuess n ->
            let
                isRight =
                    if allGuessed model then
                        Nothing
                    else
                        (model.currentQuestion |> Maybe.map (\q -> q.answer == n))
            in
                case isRight of
                    Just True ->
                        let
                            newModel =
                                Types.nextQuestion model
                        in
                            if allGuessed newModel then
                                if model.settings.autoProceed then
                                    getNewExercise { newModel | stat = countExercise newModel }
                                else
                                    { newModel | stat = countExercise newModel } ! []
                            else
                                newModel ! []

                    Just False ->
                        { model | error = True, attemps = Set.insert n model.attemps } ! []

                    Nothing ->
                        model ! []

        Exercise chords ->
            let
                newModel =
                    { model | chordsToGuess = chords }
            in
                ( { newModel | currentQuestion = Types.getQuestion newModel }
                , play model.settings.root model.settings.delay chords
                )

        StartExercises ->
            getNewExercise { model | page = ExercisePage }

        ChangeSettings f ->
            { model | settings = f model.settings } ! []

        MoveToPage page ->
            { model | page = page } ! []


keyboardMap : Model -> Char -> Msg
keyboardMap model key =
    let
        answer =
            Types.getOptionsFromModel model
                |> List.filter (\option -> option.keyMap == key)
                |> List.head
    in
        case answer of
            Just a ->
                MakeGuess a.index

            Nothing ->
                case key of
                    ' ' ->
                        NewExercise

                    'a' ->
                        Play model.chordsToGuess

                    'c' ->
                        Play Music.tonicOctave

                    _ ->
                        NoOp


subscriptions : Model -> Sub Msg
subscriptions m =
    Keyboard.presses <| keyboardMap m << Char.fromCode
