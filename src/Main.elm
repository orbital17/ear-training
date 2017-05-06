port module Main exposing (main)

import Array
import Char
import Html
import Keyboard
import Random
import Set exposing (Set)
import Chords exposing (Chord, getRandom, Note)
import Types exposing (Msg(..), Model, Page(..), allGuessed, initModel)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        --{ init = (update StartExercises initModel)
        { init = (initModel ! [])
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port playChord : Chord -> Cmd msg


port stop : () -> Cmd msg


play : Int -> Chord -> Cmd msg
play root c =
    Chords.transpose root c |> playChord


countExercise : Model -> Model
countExercise model =
    { model
        | total = model.total + 1
        , correct =
            if model.error then
                model.correct
            else
                model.correct + 1
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Play chord ->
            model ! [ play model.root chord ]

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
                    Array.get model.guessed (Array.fromList model.chord)
                        |> Maybe.map (\real -> (real % 12) == n)
            in
                case isRight of
                    Just True ->
                        let
                            newModel =
                                { model | guessed = model.guessed + 1, attemps = Set.empty }
                        in
                            (if allGuessed newModel then
                                countExercise newModel
                             else
                                newModel
                            )
                                ! []

                    Just False ->
                        { model | error = True, attemps = Set.insert n model.attemps } ! []

                    Nothing ->
                        model ! []

        Exercise c ->
            ( { model | chord = c }, play model.root c )

        StartExercises ->
            ( { model | page = ExercisePage }, Random.generate Exercise getRandom )


keyboardMap : Model -> Char -> Msg
keyboardMap model key =
    case key of
        ' ' ->
            NewExercise

        'a' ->
            Play model.chord

        'c' ->
            Play [ 0, 12 ]

        _ ->
            String.fromChar key
                |> String.toInt
                |> Result.toMaybe
                |> Maybe.andThen (\n -> Array.get (n - 1) <| Array.fromList (Chords.modeNotes model.mode))
                |> Maybe.map MakeGuess
                |> Maybe.withDefault NoOp


subscriptions : Model -> Sub Msg
subscriptions m =
    Keyboard.presses <| keyboardMap m << Char.fromCode
