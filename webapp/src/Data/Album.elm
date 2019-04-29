module Data.Album exposing (Album, albumDecoder)

import Json.Decode as D exposing (Decoder)


type alias Album =
    { albumId : String
    , albumTitle : String
    , albumYear : Int
    , albumCover : String
    , albumDescription : String
    }


albumDecoder : Decoder Album
albumDecoder =
    D.map5 Album
        (D.field "albumId" D.string)
        (D.field "albumTitle" D.string)
        (D.field "albumYear" D.int)
        (D.field "albumCover" D.string)
        (D.field "albumDescription" D.string)
