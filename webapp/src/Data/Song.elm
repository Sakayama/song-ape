module Data.Song exposing (Song, showDuration, songDecoder)

import Json.Decode as D exposing (Decoder)


type alias Song =
    { songId : String
    , artistId : String
    , albumId : String
    , title : String
    , duration : Int
    }


songDecoder : Decoder Song
songDecoder =
    D.map5 Song
        (D.field "songId" D.string)
        (D.field "artistId" D.string)
        (D.field "albumId" D.string)
        (D.field "title" D.string)
        (D.field "duration" D.int)


showDuration : Int -> String
showDuration ms =
    String.fromInt ((ms // 1000) // 60) ++ ":" ++ String.fromInt (remainderBy 60 (ms // 1000))
