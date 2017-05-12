port module Storage exposing (..)

import Json.Encode as Encode
import Json.Decode exposing (..)
import Music
import Types exposing (Settings)


enableStorage : Bool
enableStorage =
    True


intToMode : Int -> Music.Mode
intToMode i =
    case i of
        1 ->
            Music.Minor

        _ ->
            Music.Major


modeToInt : Music.Mode -> Int
modeToInt m =
    case m of
        Music.Major ->
            0

        Music.Minor ->
            1


settingsEndoder : Settings -> Encode.Value
settingsEndoder s =
    Encode.object
        [ ( "root", Encode.int s.root )
        , ( "mode", Encode.int <| modeToInt s.mode )
        , ( "guessChordName", Encode.bool s.guessChordName )
        , ( "autoProceed", Encode.bool s.autoProceed )
        , ( "chordSize", Encode.int s.chordSize )
        , ( "chordsInSequence", Encode.int s.chordsInSequence )
        , ( "delay", Encode.float s.delay )
        ]


settingsDecoder : Decoder Settings
settingsDecoder =
    map7 Settings
        (field "root" int)
        (map intToMode (field "mode" int))
        (field "guessChordName" bool)
        (field "autoProceed" bool)
        (field "chordSize" int)
        (field "chordsInSequence" int)
        (field "delay" float)


setSettings : Settings -> Cmd msg
setSettings s =
    localStorageSet <|
        if enableStorage then
            (Encode.encode 0 <| settingsEndoder s)
        else
            ""


settingsSub : (Maybe Settings -> msg) -> Sub msg
settingsSub f =
    let
        g value =
            f <| Result.toMaybe (decodeString settingsDecoder value)
    in
        localStorageSub <|
            if enableStorage then
                g
            else
                \_ -> f Nothing


port localStorageSet : String -> Cmd msg


port localStorageGet : () -> Cmd msg


port localStorageSub : (String -> msg) -> Sub msg
