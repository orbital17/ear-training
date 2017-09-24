module Music exposing (..)

import Random exposing (Generator)
import Utils


type alias Note =
    Int


type alias Chord =
    List Note


type Mode
    = Major
    | Minor


type alias Degree =
    Int


modeNotes : Mode -> List Note
modeNotes mode =
    case mode of
        Major ->
            [ 0, 2, 4, 5, 7, 9, 11 ]

        Minor ->
            [ 0, 2, 3, 5, 7, 8, 10 ]


syllable : Note -> String
syllable n =
    [ "do", "re♭", "re", "mi♭", "mi", "fa", "sol♭", "sol", "la♭", "la", "ti♭", "ti" ]
        |> Utils.get (n % 12)
        |> Maybe.withDefault "_"


noteToString : Int -> String
noteToString n =
    let
        notes =
            [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

        letter =
            Maybe.withDefault "_" (Utils.get (n % 12) notes)

        octave =
            n // 12 - 1
    in
        letter ++ (toString octave)


getNote : Mode -> Degree -> Note
getNote mode degree =
    let
        d =
            degree - 1

        nOctaves =
            d // 7

        modDegree =
            d % 7

        note =
            modeNotes mode
                |> Utils.get modDegree
                |> Maybe.withDefault -100
    in
        note + (12 * nOctaves)


getChord : Mode -> Degree -> Chord
getChord mode degree =
    List.map (\i -> getNote mode (degree + i * 2))
        (List.range 0 2)


getCadence : Mode -> List Chord
getCadence mode =
    List.map (getChord mode) [ 1, 4, 5, 1 ]


getSequenceToTonic : Mode -> List Chord -> List Chord
getSequenceToTonic mode chords =
    let
        note =
            Maybe.withDefault 0 <| Maybe.andThen List.head (List.head chords)

        notes =
            modeNotes mode

        index =
            Maybe.withDefault 0 <| Utils.indexOf (note % 12) notes

        nOctaves =
            note // 12

        descending =
            List.reverse <| List.take (index + 1) notes

        ascending =
            (List.drop index notes) ++ [ 12 ]
    in
        (if index < 4 then
            descending
         else
            ascending
        )
            |> List.map (\n -> [ n + nOctaves * 12 ])


tonicOctave : List Chord
tonicOctave =
    [ [ 0, 12 ] ]


transpose : Int -> Chord -> Chord
transpose n =
    List.map ((+) n)


toMelodic : List Chord -> List Chord
toMelodic chords =
    List.concat chords
        |> List.map (\n -> [ n ])


intervalNames : List String
intervalNames =
    [ "Unison", "Minor 2nd", "Major 2nd", "Minor 3rd", "Major 3rd", "Perfect 4th", "Tritone", "Perfect 5th", "Minor 6th", "Major 6th", "Minor 7th", "Major 7th", "Octave" ]


intervalName : Int -> String
intervalName i =
    Utils.get i intervalNames
        |> Maybe.withDefault "Unknown interval"


intervalsOptions : List Note -> List Chord
intervalsOptions scale =
    List.concatMap
        (\root ->
            List.map
                (\top -> ( root, top ))
                (scale ++ List.map ((+) 12) scale)
        )
        scale
        |> List.filter (\( root, top ) -> root < top && (top - root) <= 12)
        |> List.map (\( root, top ) -> [ root, top ])


triadNames : List String
triadNames =
    [ "maj", "min", "aug", "dim", "maj6", "min6" ]


triadName : Int -> String
triadName i =
    Utils.get i triadNames
        |> Maybe.withDefault "Unknown interval"


triadMasks : List ( Int, Int )
triadMasks =
    [ ( 4, 3 ), ( 3, 4 ), ( 4, 4 ), ( 3, 3 ), ( 3, 5 ), ( 4, 5 ), ( 7, 9 ), ( 7, 8 ), ( 8, 7 ), ( 9, 7 ), ( 5, 3 ), ( 5, 4 ), ( 8, 9 ), ( 9, 8 ) ]


triadIndex : ( Int, Int ) -> Int
triadIndex mask =
    Maybe.withDefault 0 <| Utils.indexOf mask triadMasks


triadOptions : List Note -> List Chord
triadOptions scale =
    let
        extendedScale =
            scale ++ List.map ((+) 12) scale ++ List.map ((+) 24) scale

        makeTriad note ( a, b ) =
            [ note, note + a, note + a + b ]

        inScale chord =
            List.all (flip List.member extendedScale) chord

        chordsFromNote n =
            List.map (makeTriad n) triadMasks
                |> List.filter inScale
    in
        List.concatMap chordsFromNote scale


chordName : Chord -> String
chordName c =
    case c of
        [ a ] ->
            "Unison"

        [ a, b ] ->
            Utils.get (b - a) intervalNames
                |> Maybe.withDefault "_"

        [ a, b, c ] ->
            Utils.get (triadIndex ( b - a, c - b )) triadNames
                |> Maybe.withDefault "_"

        _ ->
            "_"


getRandom : Mode -> Int -> Generator Chord
getRandom mode chordSize =
    let
        optionsList =
            case chordSize of
                1 ->
                    List.map (\n -> [ n ]) <| modeNotes mode

                2 ->
                    intervalsOptions <| modeNotes mode

                3 ->
                    triadOptions <| modeNotes mode

                _ ->
                    []
    in
        optionsList
            |> Utils.randomFromList
            |> Random.map (Maybe.withDefault [])
