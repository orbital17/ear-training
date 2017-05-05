module Chords exposing (..)

import Random exposing (Generator)

type alias Note = Int

type alias Chord = List Note

type Mode = Major | Minor

modeNotes : Mode -> List Note
modeNotes mode =
    case mode of
        Major ->
            List.map ((+) 60) [0, 2, 4, 5, 7, 9, 11]
        Minor ->
            [0, 2, 3, 5, 7, 8, 10]


randomFromList : List a -> Generator (Maybe a)
randomFromList list =
    let
        gen = Random.int 0 (List.length list - 1)
        get i l = List.head <| List.drop i l
    in
    Random.map (\index -> get index list) gen


randomNote : Mode -> Generator (Maybe Note)
randomNote mode = randomFromList <| modeNotes mode

