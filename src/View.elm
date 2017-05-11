module View exposing (..)

import Html exposing (li, div, Html, text, button, p, span, input)
import Html.Attributes exposing (class, classList, type_, checked)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Music exposing (Chord, Note, Mode(..))
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
        [ b (Play (Music.getCadence m.settings.mode)) "Hear cadence" False
        , b (Play <| Music.toMelodic m.chordsToGuess) "Hear sequencially" False
        , b (Play [ [ 0, 12 ] ]) "Hear tonic [c]" False
        , b (Play m.chordsToGuess) "Hear again [a]" False
        , b (NewExercise) "Next [spacebar]" (not (allGuessed m))
        , b (MoveToPage SettingsPage) "Settings" False
        ]


answerButton : Model -> Types.AnswerOption -> Html Msg
answerButton m answerOption =
    let
        attemted =
            Set.member answerOption.index m.attemps

        isRightAnswer =
            m.currentQuestion
                |> Maybe.map (\q -> q.answer == answerOption.index)
                |> Maybe.withDefault False
    in
        button
            [ classList
                [ ( "button", True )
                , ( "answer-button", True )
                , ( "is-danger", attemted )
                , ( "is-info", not attemted )
                , ( "is-success"
                  , (allGuessed m) && isRightAnswer
                  )
                ]
            , onClick (MakeGuess answerOption.index)
            ]
            [ text <| answerOption.name ++ " [" ++ String.fromChar answerOption.keyMap ++ "]" ]


answerButtons : Model -> List (Html Msg)
answerButtons m =
    List.map (answerButton m) (Types.getOptionsFromModel m)


answerLine : Model -> List (Html Msg)
answerLine m =
    let
        noteCell chordIndex noteIndex note =
            if
                (chordIndex < m.guessed.chords)
                    || (chordIndex == m.guessed.chords && noteIndex < m.guessed.notes)
            then
                Music.syllable note
            else if chordIndex == m.guessed.chords && noteIndex == m.guessed.notes then
                " ? "
            else
                ""

        chordNameCell chordIndex chord =
            if (chordIndex < m.guessed.chords) then
                Music.chordName chord
            else if chordIndex == m.guessed.chords && (List.length chord) == m.guessed.notes then
                " ? "
            else
                ""

        chordColumn chordIndex chord =
            chord
                |> List.indexedMap (noteCell chordIndex)
                |> flip List.append
                    (if m.settings.guessChordName then
                        [ chordNameCell chordIndex chord ]
                     else
                        []
                    )
                |> List.map (\s -> span [ class "is-large label" ] [ text s ])
                |> List.reverse
    in
        m.chordsToGuess
            |> List.indexedMap chordColumn
            |> List.map (div [ class "answer-chord-column" ])


quiz : Model -> List (Html Msg)
quiz m =
    [ div [ class "block" ]
        [ text
            (toString m.stat.correct
                ++ " of "
                ++ toString m.stat.total
                ++ " correct"
            )
        ]
    , div [ class "block" ] <| answerButtons m
    , div [ class "block" ] <| answerLine m
    ]


settingsView : Model -> List (Html Msg)
settingsView m =
    let
        b msg title isSelected =
            p [ class "control" ]
                [ button
                    [ classList
                        [ ( "button is-info", True )
                        , ( "is-outlined", not isSelected )

                        --, ( "is-active", isSelected )
                        ]
                    , onClick msg
                    ]
                    [ text title ]
                ]

        switch msg checked =
            Html.label [ class "switch" ]
                [ input
                    [ type_ "checkbox"
                    , Html.Events.onCheck (\_ -> msg)
                    , Html.Attributes.checked checked
                    ]
                    []
                , div [ class "slider round" ] []
                ]

        settingField title inputs =
            div [ class "settings-field" ] [ div [ class "setting-label" ] [ text title ], inputs ]
    in
        [ settingField "Scale" <|
            div [ class "field has-addons" ]
                [ b (ChangeSettings (\s -> { s | mode = Major })) "Major" (m.settings.mode == Major)
                , b (ChangeSettings (\s -> { s | mode = Minor })) "Minor" (m.settings.mode == Minor)
                ]
        , settingField "Auto proceed" <|
            div [ class "field" ]
                [ switch (ChangeSettings (\s -> { s | autoProceed = not s.autoProceed }))
                    m.settings.autoProceed
                ]
        , settingField "Name guess" <|
            div [ class "field has-addons" ]
                [ switch (ChangeSettings (\s -> { s | guessChordName = not s.guessChordName }))
                    m.settings.guessChordName
                ]
        , settingField "Delay" <|
            div [ class "field has-addons" ]
                [ b (ChangeSettings (\s -> { s | delay = 0.4 })) "0.4" (m.settings.delay == 0.4)
                , b (ChangeSettings (\s -> { s | delay = 0.7 })) "0.7" (m.settings.delay == 0.7)
                , b (ChangeSettings (\s -> { s | delay = 1 })) "1" (m.settings.delay == 1)
                ]
        , settingField "Notes in chord" <|
            div [ class "field has-addons" ]
                [ b (ChangeSettings (\s -> { s | chordSize = 1 })) "1" (m.settings.chordSize == 1)
                , b (ChangeSettings (\s -> { s | chordSize = 2 })) "2" (m.settings.chordSize == 2)
                ]
        , settingField "Chords number" <|
            div [ class "field has-addons" ] <|
                List.map
                    (\i ->
                        b (ChangeSettings (\s -> { s | chordsInSequence = i }))
                            (toString i)
                            (m.settings.chordsInSequence == i)
                    )
                    (List.range 1 4)
        , settingField "Root note" <|
            div [ class "field" ]
                [ b (ChangeSettings (\s -> { s | root = s.root - 1 })) "-" False
                , span [ class "" ] [ text (Music.noteToString m.settings.root) ]
                , b (ChangeSettings (\s -> { s | root = s.root + 1 })) "+" False
                ]
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

        SettingsPage ->
            div [ class "section columns" ]
                [ div [ class "column is-7 is-offset-1" ] (settingsView m)
                , div [ class "column" ]
                    [ button
                        [ class "button is-info is-outlined"
                        , (onClick <| MoveToPage ExercisePage)
                        ]
                        [ text "Back" ]
                    ]
                ]


view : Model -> Html Msg
view model =
    div [ class "container" ] [ nav, (content model) ]
