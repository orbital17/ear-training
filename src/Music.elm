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
    [ "do", "re♭", "re", "mi♭", "mi", "fa", "sol♭", "sol", "la♭", "la", "si♭", "si" ]
        |> Utils.get (n % 12)
        |> Maybe.withDefault "_"


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


tonicOctave : List Chord
tonicOctave =
    [ [ 0, 12 ] ]


intervalNames : List String
intervalNames =
    [ "Unison", "Minor 2nd", "Major 2nd", "Minor 3rd", "Major 3rd", "Perfect 4th", "Tritone", "Perfect 5th", "Minor 6th", "Major 6th", "Minor 7th", "Major 7th", "Octave" ]


intervalName : Int -> String
intervalName i =
    Utils.get i intervalNames
        |> Maybe.withDefault "Unknown interval"


chordName : Chord -> String
chordName c =
    case c of
        [ a ] ->
            "Unison"

        [ a, b ] ->
            Utils.get (b - a) intervalNames
                |> Maybe.withDefault "_"

        _ ->
            "_"


transpose : Int -> Chord -> Chord
transpose n =
    List.map ((+) n)


toMelodic : List Chord -> List Chord
toMelodic chords =
    List.concat chords
        |> List.map (\n -> [ n ])


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


getRandom : Mode -> Int -> Generator Chord
getRandom mode chordSize =
    let
        optionsList =
            case chordSize of
                1 ->
                    List.map (\n -> [ n ]) <| modeNotes mode

                2 ->
                    intervalsOptions <| modeNotes mode

                _ ->
                    []
    in
        optionsList
            |> Utils.randomFromList
            |> Random.map (Maybe.withDefault [])
