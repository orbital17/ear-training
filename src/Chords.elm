module Chords exposing (..)

import Random exposing (Generator)
import Utils


type alias Note =
    Int


type alias Chord =
    List Note


type Mode
    = Major
    | Minor


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


getNote : Mode -> Int -> Note
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


getChord : Mode -> Int -> Chord
getChord mode degree =
    List.map (\i -> getNote mode (degree + i * 2))
        (List.range 0 2)


getCadence : Mode -> List Chord
getCadence mode =
    List.map (getChord mode) [ 1, 4, 5, 1 ]


randomFromList : List a -> Generator (Maybe a)
randomFromList list =
    let
        gen =
            Random.int 0 (List.length list - 1)
    in
        Random.map (\index -> Utils.get index list) gen


randomNote : Mode -> Generator Note
randomNote mode =
    Random.map (Maybe.withDefault -100) (randomFromList (modeNotes mode))


transpose : Int -> Chord -> Chord
transpose n =
    List.map ((+) n)


intervals : List Note -> List Chord
intervals scale =
    List.concatMap
        (\root ->
            List.map
                (\top -> ( root, top ))
                (scale ++ List.map ((+) 12) scale)
        )
        scale
        |> List.filter (\( root, top ) -> root < top && (top - root) <= 12)
        |> List.map (\( root, top ) -> [ root, top ])


randomInterval : Mode -> Generator Chord
randomInterval =
    Random.map (Maybe.withDefault [ 0, 0 ]) << randomFromList << intervals << modeNotes


getRandom : Generator Chord
getRandom =
    randomInterval Major
