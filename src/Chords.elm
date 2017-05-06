module Chords exposing (..)

import Random exposing (Generator)
import Array exposing (Array)


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
        |> Array.fromList
        |> Array.get (n % 12)
        |> Maybe.withDefault "_"


randomFromList : List a -> Generator (Maybe a)
randomFromList list =
    let
        gen =
            Random.int 0 (List.length list - 1)

        get i l =
            List.head <| List.drop i l
    in
        Random.map (\index -> get index list) gen


randomNote : Mode -> Generator Note
randomNote mode =
    Random.map (Maybe.withDefault 0) <| randomFromList <| modeNotes mode


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
