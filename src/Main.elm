port module Main exposing (main)

import Char
import Html
import Keyboard
import Random
import Set exposing (Set)
import Chords exposing (Chord, Note)
import Types exposing (Msg(..), Model, Page(..), allGuessed, initModel, Statistics)
import View exposing (view)
import Utils


main : Program Never Model Msg
main =
    Html.program
        { init = (update StartExercises initModel)

        --{ init = (initModel ! [])
        , view = view
        , update = debugUpdate
        , subscriptions = subscriptions
        }


port playChordsPort : ( List Chord, Float ) -> Cmd msg


play : Int -> Float -> List Chord -> Cmd msg
play root timeInterval chords =
    playChordsPort ( List.map (Chords.transpose root) chords, timeInterval )


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


getNewExercise : Model -> ( Model, Cmd Msg )
getNewExercise model =
    ( { model | guessed = 0, error = False }
    , Random.generate Exercise (Chords.randomInterval model.settings.mode)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Play chords ->
            model ! [ play model.settings.root 0.5 chords ]

        NewExercise ->
            if allGuessed model then
                getNewExercise model
            else
                (model ! [])

        MakeGuess n ->
            let
                isRight =
                    Utils.get model.guessed model.questions
                        |> Maybe.map (\q -> q.answer == n)
            in
                case isRight of
                    Just True ->
                        let
                            newModel =
                                { model | guessed = model.guessed + 1, attemps = Set.empty }
                        in
                            if allGuessed newModel then
                                getNewExercise { newModel | stat = countExercise newModel }
                            else
                                newModel ! []

                    Just False ->
                        { model | error = True, attemps = Set.insert n model.attemps } ! []

                    Nothing ->
                        model ! []

        Exercise chords ->
            ( { model
                | chordsToGuess = chords
                , questions = Types.getQuestions model.settings.guessChordName chords
              }
            , play model.settings.root 0 chords
            )

        StartExercises ->
            getNewExercise { model | page = ExercisePage }


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
                        Play [ [ 0, 12 ] ]

                    _ ->
                        NoOp


subscriptions : Model -> Sub Msg
subscriptions m =
    Keyboard.presses <| keyboardMap m << Char.fromCode
