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


actionButton : Msg -> String -> String -> Bool -> Bool -> Html Msg
actionButton msg title keyBinding isDisabled isHidden =
    button
        [ classList [ ( "pure-button is-info is-outlined", True ), ( "hidden", isHidden ) ]
        , onClick msg
        , Html.Attributes.disabled isDisabled
        ]
        [ text title, span [ class "key-binding" ] [ text keyBinding ] ]


actionButtons : Model -> List (Html Msg)
actionButtons m =
    let
        b =
            actionButton
    in
        [ b (Play m.chordsToGuess) "Hear again" "[x]" False False
        , b (Play [ [ 0, 12 ] ]) "Hear tonic" "[c]" False False
        , b (PlayCustomDelay 0.5 (Music.getCadence m.settings.mode))
            "Hear cadence"
            ""
            False
            False
        , b (Play <| Music.toMelodic m.chordsToGuess)
            "Hear sequentially"
            ""
            False
            (m.settings.chordSize == 1)
        , b (PlayCustomDelay 0.5 (Music.getSequenceToTonic m.settings.mode m.chordsToGuess))
            "Hear sequence to tonic"
            ""
            (not (allGuessed m))
            (m.settings.chordSize
                > 1
                || m.settings.chordsInSequence
                > 1
                || m.settings.autoProceed
            )
        , b (NewExercise) "Next" "[spacebar]" (not (allGuessed m)) m.settings.autoProceed
        , b (MoveToPage <| SettingsPage m.page) "Settings" "" False False
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
                [ ( "pure-button", True )
                , ( "answer-button", True )
                , ( "is-danger", attemted )
                , ( "is-info", not attemted )
                , ( "is-success"
                  , (allGuessed m) && isRightAnswer
                  )
                ]
            , onClick (MakeGuess answerOption.index)
            ]
            [ span [] [ text answerOption.name ]
            , span [ class "key-binding" ] [ text <| "[" ++ String.fromChar answerOption.keyMap ++ "]" ]
            ]


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
                " · "

        chordNameCell chordIndex chord =
            if (chordIndex < m.guessed.chords) then
                Music.chordName chord
            else if chordIndex == m.guessed.chords && (List.length chord) == m.guessed.notes then
                " ? "
            else
                " · "

        chordColumn chordIndex chord =
            chord
                |> List.indexedMap (noteCell chordIndex)
                |> flip List.append
                    (if m.settings.guessChordName then
                        [ chordNameCell chordIndex chord ]
                     else
                        []
                    )
                |> List.map (\s -> span [] [ text s ])
    in
        m.chordsToGuess
            |> List.indexedMap chordColumn
            |> List.map (div [ class "answer-chord-column" ])


quiz : Model -> List (Html Msg)
quiz m =
    [ div [ class "statistics-line" ]
        [ text
            (toString m.stat.correct
                ++ " of "
                ++ toString m.stat.total
                ++ " correct"
            )
        ]
    , div [ class "answer-line" ] <| answerLine m
    , div [ class "answer-buttons" ] <| answerButtons m
    , div [ class "buttons-panel" ] (actionButtons m)
    ]


settingsView : Model -> List (Html Msg)
settingsView m =
    let
        b msg title isSelected =
            button
                [ classList
                    [ ( "pure-button is-info", True )
                    , ( "is-outlined", not isSelected )
                    ]
                , onClick msg
                ]
                [ text title ]

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
            div [ class "settings-field" ] [ div [ class "setting-label" ] [ text title ], div [] inputs ]

        s =
            m.settings
    in
        [ settingField "Scale" <|
            [ b (ChangeSettings { s | mode = Major }) "Major" (s.mode == Major)
            , b (ChangeSettings { s | mode = Minor }) "Minor" (s.mode == Minor)
            ]
        , settingField "Root note" <|
            [ b (ChangeSettings { s | root = s.root - 1 }) "-" False
            , span [ class "root-note" ] [ text (Music.noteToString s.root) ]
            , b (ChangeSettings { s | root = s.root + 1 }) "+" False
            ]
        , settingField "Notes in chord" <|
            [ b (ChangeSettings { s | chordSize = 1 }) "1" (s.chordSize == 1)
            , b (ChangeSettings { s | chordSize = 2 }) "2" (s.chordSize == 2)
            , b (ChangeSettings { s | chordSize = 3 }) "3" (s.chordSize == 3)
            ]
        , settingField "Chords number" <|
            List.map
                (\i ->
                    b (ChangeSettings { s | chordsInSequence = i })
                        (toString i)
                        (s.chordsInSequence == i)
                )
                (List.range 1 4)
        , settingField "Auto proceed" <|
            [ switch (ChangeSettings { s | autoProceed = not s.autoProceed }) s.autoProceed ]
        , settingField "Name guess" <|
            [ switch (ChangeSettings { s | guessChordName = not s.guessChordName }) s.guessChordName ]
        , settingField "Delay" <|
            [ b (ChangeSettings { s | delay = 0.4 }) "0.4" (s.delay == 0.4)
            , b (ChangeSettings { s | delay = 0.7 }) "0.7" (s.delay == 0.7)
            , b (ChangeSettings { s | delay = 1 }) "1" (s.delay == 1)
            ]
        ]


content : Model -> Html Msg
content m =
    case m.page of
        ExercisePage ->
            div [ class "exercise-screen" ] (quiz m)

        MainPage ->
            div [ class "start-screen" ]
                [ div [ class "buttons" ]
                    [ button [ class "pure-button pure-button-primary", (onClick StartExercises) ] [ text "Notes and chords" ]
                    , button [ class "pure-button pure-button-primary", (onClick StartProgressionExercise) ] [ text "Chord progressions" ]
                    , button [ class "pure-button pure-button-primary", (onClick StartNoteInChordExercise) ] [ text "Note in chord" ]
                    ]
                ]

        SettingsPage page ->
            div [ class "settings-screen" ]
                [ div [ class "settings-list" ] (settingsView m)
                , div [ class "back-button" ]
                    [ button
                        [ class "button is-info is-outlined"
                        , (onClick <| MoveToPage page)
                        ]
                        [ text "Back" ]
                    ]
                ]

        ChordProgressionsPage ->
            div [ class "exercise-screen" ] (quiz m)

        NoteInChordPage ->
            div [ class "exercise-screen" ] (quiz m)


view : Model -> Html Msg
view model =
    div [ class "container" ] [ (content model) ]
