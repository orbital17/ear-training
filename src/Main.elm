port module Main exposing (main)

import Char
import Html
import Keyboard
import Random
import Set exposing (Set)
import Chords exposing (Chord, getRandom, Note)
import Types exposing (Msg(..), Model, Page(..), allGuessed, initModel, Statistics)
import View exposing (view)
import Utils


main : Program Never Model Msg
main =
    Html.program
        { init = (update StartExercises initModel)

        --{ init = (initModel ! [])
        , view = view
        , update = update
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        PlayOne chord ->
            model ! [ play model.settings.root 0 [ chord ] ]

        Play chords ->
            model ! [ play model.settings.root 0.5 chords ]

        NewExercise ->
            if allGuessed model then
                ( { model | guessed = 0, error = False }
                , Random.generate Exercise getRandom
                )
            else
                (model ! [])

        MakeGuess n ->
            let
                isRight =
                    Utils.get model.guessed model.chord
                        |> Maybe.map (\real -> (real % 12) == n)
            in
                case isRight of
                    Just True ->
                        let
                            newModel =
                                { model | guessed = model.guessed + 1, attemps = Set.empty }
                        in
                            (if allGuessed newModel then
                                { newModel | stat = countExercise newModel }
                             else
                                newModel
                            )
                                ! []

                    Just False ->
                        { model | error = True, attemps = Set.insert n model.attemps } ! []

                    Nothing ->
                        model ! []

        Exercise c ->
            ( { model | chord = c }, play model.settings.root 0 [ c ] )

        StartExercises ->
            ( { model | page = ExercisePage }, Random.generate Exercise getRandom )


keyboardMap : Model -> Char -> Msg
keyboardMap model key =
    case key of
        ' ' ->
            NewExercise

        'a' ->
            PlayOne model.chord

        'c' ->
            PlayOne [ 0, 12 ]

        _ ->
            String.fromChar key
                |> String.toInt
                |> Result.toMaybe
                |> Maybe.andThen (\n -> Utils.get (n - 1) (Chords.modeNotes model.settings.mode))
                |> Maybe.map MakeGuess
                |> Maybe.withDefault NoOp


subscriptions : Model -> Sub Msg
subscriptions m =
    Keyboard.presses <| keyboardMap m << Char.fromCode
