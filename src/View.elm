module View exposing (..)

import Html exposing (li, div, Html, text, button, p, span)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Chords exposing (Chord, Note)
import Types exposing (Msg(..), Model, Page(..), allGuessed)


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
        [ b (Play (Chords.getCadence m.settings.mode)) "Hear cadence" False
        , b (PlayOne [ 0, 12 ]) "Hear tonic [c]" False
        , b (PlayOne m.chord) "Hear again [a]" False
        , b (NewExercise) "Next [spacebar]" (not (allGuessed m))
        ]


noteButtons : Model -> List (Html Msg)
noteButtons m =
    Chords.modeNotes m.settings.mode
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
    [ div [ class "block" ] [ text (toString m.stat.correct ++ " of " ++ toString m.stat.total ++ " correct") ]
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
