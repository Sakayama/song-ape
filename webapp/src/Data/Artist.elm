module Data.Artist exposing (Artist, artistDecoder)

import Json.Decode as D exposing (Decoder)


type alias Artist =
    { artistId : String
    , artistName : String
    , artistPhoto : String
    , artistDescription : String
    }


artistDecoder : Decoder Artist
artistDecoder =
    D.map4 Artist
        (D.field "artistId" D.string)
        (D.field "artistName" D.string)
        (D.field "artistPhoto" D.string)
        (D.field "artistDescription" D.string)
