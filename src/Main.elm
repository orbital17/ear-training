port module Main exposing (main)

import Array
import Char
import Html exposing (li, div, Html, text, button, p, span)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Keyboard
import Random
import Set exposing (Set)
import Chords exposing (Chord, getRandom, Note)


main : Program Never Model Msg
main =
    Html.program
        { init = (update StartExercises initModel)

        --{ init = (initModel ! [])
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
        | total = model.total + 1
        , correct =
            if model.error then
                model.correct
            else
                model.correct + 1
    }


allGuessed : Model -> Bool
allGuessed model =
    model.guessed == List.length model.chord


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



-- VIEW


nav : Html Msg
nav =
    div [ class "nav" ]
        [ Html.p [ class "nav-center title" ] [ text "Ear Training" ]
        ]


rightPanelButton : Msg -> String -> Bool -> Html Msg
rightPanelButton msg title isDisabled =
    p [ class "field" ]
        [ button
            [ class "button is-info is-outlined"
            , onClick msg
            , Html.Attributes.disabled isDisabled
            ]
            [ text title ]
        ]


rightPanelButtons : Model -> List (Html Msg)
rightPanelButtons m =
    let
        b =
            rightPanelButton
    in
        [ b (Play [ 0, 12 ]) "Hear tonic [c]" False
        , b (Play m.chord) "Hear again [a]" False
        , b (NewExercise) "Next [spacebar]" (not (allGuessed m))
        ]


noteButtons : Model -> List (Html Msg)
noteButtons m =
    Chords.modeNotes m.mode
        |> List.map
            (\note ->
                button
                    [ class <|
                        "button note "
                            ++ (if Set.member note m.attemps then
                                    "is-danger"
                                else
                                    "is-info"
                               )
                    , onClick (MakeGuess note)
                    ]
                    [ text (Chords.syllable note) ]
            )


answerLine : Model -> List (Html Msg)
answerLine m =
    List.take m.guessed m.chord
        |> List.map Chords.syllable
        |> flip (++)
            (if allGuessed m then
                []
             else
                [ "?" ]
            )
        |> List.map (\s -> span [ class "is-large label" ] [ text s ])
        |> List.reverse


quiz : Model -> List (Html Msg)
quiz m =
    [ div [ class "block" ] [ text (toString m.correct ++ " of " ++ toString m.total ++ " correct") ]
    , div [ class "block" ] <| noteButtons m
    , div [ class "block" ] <| answerLine m
    ]


content : Model -> Html Msg
content m =
    case m.page of
        ExercisePage ->
            div [ class "section columns" ]
                [ div [ class "column is-7 is-offset-1" ] (quiz m)
                , div [ class "column" ] (rightPanelButtons m)
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
