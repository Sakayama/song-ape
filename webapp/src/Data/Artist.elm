module Data.Artist exposing (Artist, artistDecoder)

import Json.Decode as D exposing (Decoder)


type alias Artist =
    { artistId : String
    , artistName : String
    }


artistDecoder : Decoder Artist
artistDecoder =
    D.map2 Artist
        (D.field "artistId" D.string)
        (D.field "artistName" D.string)
