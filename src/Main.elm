port module Main exposing (main)

import Html exposing (li, div, Html, text, button, p, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Char
import Array
import Random
import Keyboard
import Chords exposing (Chord, getRandom, Note)


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
    { root : Int
    , chord : Chord
    , total : Int
    , correct : Int
    , error : Bool
    , guessed : Int
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
    , mode = Chords.Major
    , page = MainPage
    }


port playChord : Chord -> Cmd msg


port stop : () -> Cmd msg


play : Int -> Chord -> Cmd msg
play root c =
    Chords.transpose root c |> playChord



-- UPDATE


type Msg
    = NoOp
    | Play Chord
    | NewExercise
    | Exercise Chord
    | MakeGuess Note
    | StartExercises


countExercise : Model -> Model
countExercise model =
    { model
        | guessed = 0
        , error = False
        , total = model.total + 1
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
            if model.guessed /= List.length model.chord then
                (model ! [])
            else
                ( countExercise model
                , Random.generate Exercise getRandom
                )

        MakeGuess n ->
            let
                isRight =
                    Array.get model.guessed (Array.fromList model.chord)
                        |> Maybe.map (\real -> (real % 12) == n)
            in
                case isRight of
                    Just True ->
                        { model | guessed = model.guessed + 1 } ! []

                    Just False ->
                        { model | error = True } ! []

                    Nothing ->
                        model ! []

        Exercise c ->
            ( { model | chord = c }, play model.root c )

        StartExercises ->
            ( { model | page = ExercisePage }, Random.generate Exercise getRandom )



-- VIEW


nav : Html Msg
nav =
    div [ class "nav" ]
        [ Html.p [ class "nav-center title" ] [ text "Ear Training" ]
        ]


buttons : Model -> List (Html Msg)
buttons m =
    let
        b msg title =
            p [ class "field" ]
                [ button [ class "button", onClick msg ] [ text title ]
                ]
    in
        [ b (Play [ 0, 12 ]) "Hear tonic [c]"
        , b (Play m.chord) "Hear again [a]"
        , b (NewExercise) "Next [spacebar]"
        ]


noteButtons : Model -> List (Html Msg)
noteButtons m =
    Chords.modeNotes m.mode
        |> List.map (\note -> button [ class "button", onClick (MakeGuess note) ] [ text (Chords.syllable note) ])


answerLine : Model -> String
answerLine m =
    List.take m.guessed m.chord
        |> List.map Chords.syllable
        |> String.join "  "
        |> flip (++)
            (if m.guessed == List.length m.chord then
                ""
             else
                "  ?"
            )


quiz : Model -> List (Html Msg)
quiz m =
    [ div [] [ text (toString m.correct ++ " of " ++ toString m.total ++ " correct") ]
    , div [] <| noteButtons m
    , span [ class "is-large box" ] [ text <| answerLine m ]
    ]


content : Model -> Html Msg
content m =
    case m.page of
        ExercisePage ->
            div [ class "section columns" ]
                [ div [ class "column is-5 is-offset-3" ] (quiz m)
                , div [ class "column" ] (buttons m)
                ]

        MainPage ->
            div [ class "has-text-centered" ]
                [ button [ class "button is-primary is-large", (onClick StartExercises) ] [ text "Start" ] ]


view : Model -> Html Msg
view model =
    div [ class "container" ] [ nav, (content model) ]


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
