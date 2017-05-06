module Utils exposing (..)

import List


get : Int -> List a -> Maybe a
get i l =
    List.head (List.drop i l)
