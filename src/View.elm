module View exposing (..)

import Html exposing (li, div, Html, text, button, p, span)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Chords exposing (Chord, Note)
import Types exposing (Msg(..), Model, Page(..), allGuessed)
import Utils


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
        , b (Play [ [ 0, 12 ] ]) "Hear tonic [c]" False
        , b (Play m.chordsToGuess) "Hear again [a]" False
        , b (NewExercise) "Next [spacebar]" (not (allGuessed m))
        ]


answerButton : Model -> Types.AnswerOption -> Html Msg
answerButton m answerOption =
    let
        attemted =
            Set.member answerOption.index m.attemps
    in
        button
            [ classList
                [ ( "button", True )
                , ( "answer-button", True )
                , ( "is-danger", attemted )
                , ( "is-info", not attemted )
                ]
            , onClick (MakeGuess answerOption.index)
            ]
            [ text <| answerOption.name ++ " [" ++ String.fromChar answerOption.keyMap ++ "]" ]


answerButtons : Model -> List (Html Msg)
answerButtons m =
    List.map (answerButton m) (Types.getOptionsFromModel m)


answerLine : Model -> List (Html Msg)
answerLine m =
    Utils.get 0 m.chordsToGuess
        |> Maybe.withDefault []
        |> List.take m.guessed
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
    , div [ class "block" ] <| answerButtons m
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
