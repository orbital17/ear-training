port module Main exposing (main)

import Html exposing (li, div, Html, text, button, p, span)
import Html.Attributes exposing (class)
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
    { root : Int
    , chord : Chord
    }


initModel : Model
initModel =
    { root = 48
    , chord = []
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Play chord ->
            model ! [ play model.root chord ]

        NewExercise ->
            ( model, Random.generate Exercise getRandom )

        Exercise c ->
            ( { model | chord = c }, play model.root c )



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


quiz : Model -> List (Html Msg)
quiz m =
    [ span [] [ text "Notes:" ]
    , span [] [ text "fa" ]
    , span [] [ text "?" ]
    , span [] [ text "?" ]
    ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ nav
        , div [ class "section columns" ]
            [ div [ class "column is-5 is-offset-3" ] (quiz model)
            , div [ class "column" ] (buttons model)
            ]
        ]


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
            NoOp


subscriptions : Model -> Sub Msg
subscriptions m =
    Keyboard.presses <| keyboardMap m << Char.fromCode
