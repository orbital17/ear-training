module Utils exposing (..)

import List
import Random exposing (Generator)


get : Int -> List a -> Maybe a
get i l =
    List.head (List.drop i l)


randomFromList : List a -> Generator (Maybe a)
randomFromList list =
    let
        gen =
            Random.int 0 (List.length list - 1)
    in
        Random.map (\index -> get index list) gen


indexOf : a -> List a -> Maybe Int
indexOf a list =
    List.indexedMap (,) list
        |> List.filter (\( _, snd ) -> snd == a)
        |> List.head
        |> Maybe.map (\( fst, _ ) -> fst)
