module Data.Item exposing (AlbumDatagram, Item(..), SongDatagram, itemPrice, splitItems, itemDecoder)

import Data.Album exposing (Album, albumDecoder)
import Data.Artist exposing (Artist, artistDecoder)
import Data.Song exposing (Song, songDecoder)
import Json.Decode as D exposing (Decoder)


type Item
    = ItemSongDatagram SongDatagram
    | ItemAlbumDatagram AlbumDatagram


type alias SongDatagram =
    { song : Song
    , artist : Maybe Artist
    , album : Maybe Album
    , price : Float
    }


type alias AlbumDatagram =
    { album : Album
    , artist : Maybe Artist
    , songs : List Song
    , price : Float
    }


itemDecoder : Decoder Item
itemDecoder =
    D.oneOf
        [ D.map ItemAlbumDatagram albumDatagramDecoder
        , D.map ItemSongDatagram songDatagramDecoder
        ]


songDatagramDecoder : Decoder SongDatagram
songDatagramDecoder =
    D.map4 SongDatagram
        (D.field "song" songDecoder)
        (D.field "artist" (D.maybe artistDecoder))
        (D.field "album" (D.maybe albumDecoder))
        (D.field "price" D.float)


albumDatagramDecoder : Decoder AlbumDatagram
albumDatagramDecoder =
    D.map4 AlbumDatagram
        (D.field "album" albumDecoder)
        (D.field "artist" (D.maybe artistDecoder))
        (D.field "songs" (D.list songDecoder))
        (D.field "price" D.float)


splitItems : List Item -> ( List SongDatagram, List AlbumDatagram )
splitItems items =
    List.foldl
        (\item ( songs, albums ) ->
            case item of
                ItemSongDatagram sd ->
                    ( sd :: songs, albums )

                ItemAlbumDatagram ad ->
                    ( songs, ad :: albums )
        )
        ( [], [] )
        items


itemPrice : Item -> Float
itemPrice item =
    case item of
        ItemSongDatagram x ->
            x.price

        ItemAlbumDatagram x ->
            x.price
