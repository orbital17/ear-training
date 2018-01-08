module Utils exposing (..)

import List exposing (length, drop, take)
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


permutations : List a -> List (List a)
permutations list =
    if length list < 2 then
        [ list ]
    else
        let
            prev =
                permutations (drop 1 list)

            h =
                take 1 list

            put l =
                List.map
                    (\index -> List.concat [ take index l, h, drop index l ])
                    (List.range 0 (length l))
        in
            List.map put prev
                |> List.concat
