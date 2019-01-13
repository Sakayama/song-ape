module Order (Order(..), OrderDatagram, AlbumDatagram, createOrder, orderOnlyAlbums, serializeOrder, serializeAlbum) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Song as S
import qualified Artist
import qualified Album
import Util

-- (songId, price)
type SongItem = (String, Float)

-- (song, artist, album, price)
type SongDatagram = (S.Song, Maybe Artist.Artist, Maybe Album.Album, Float)

data AlbumItem = AlbumItem { albumId :: String, albumPrice :: Float }

type AlbumDatagram = (Album.Album, Maybe Artist.Artist, [S.Song], Float)

type Item = SongItem -- or AlbumItem

type ItemDatagram = SongDatagram -- or AlbumDatagram

data Order = Order { orderId :: Int, content :: [Item] }

-- TODO: find a proper name for Contract
type OrderDatagram = (Int, [ItemDatagram])

-- takes a list of prices, a list of previous orders and a list of ids
-- returns an order in case if it's not empty
-- absence of data for any specific song does not lead to failure, instead it will be skipped
-- we suppose that if we have price, then we'll definitely have a song
createOrder :: [(String, Float)] -> [Order] -> [String] -> Maybe Order
createOrder songPrices previousOrders ids = 
  if null orderContent then Nothing else Just (Order nextId orderContent)
  where orderContent = catMaybes $ findPrice <$> nub ids
        findPrice id = find (\(x, _) -> x == id) songPrices
        nextId = (fromMaybe 0 . fmap succ . safeMaximum . fmap orderId) previousOrders

orderOnlyAlbums :: [(String, Float)] -> [String] -> Maybe [AlbumItem]
orderOnlyAlbums albumPrices ids = 
  if null items then Nothing else Just items -- refactor if + catMaybes to a specialized function
  where items = catMaybes $ findPrice <$> nub ids
        findPrice id = (\(x, y) -> AlbumItem x y) <$> find (\(x, _) -> x == id) albumPrices

-- prepares order to be transmitted to frontend
serializeOrder :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> Order -> OrderDatagram
serializeOrder artists albums songs (Order orderId orderContent) = (orderId, serializedContent) where
  serializedContent = [
    (song, M.lookup (S.artistId song) artists, M.lookup (S.albumId song) albums, price) |
    song <- M.elems songs, 
    (id, price) <- orderContent, 
    S.songId song == id
    ]

serializeAlbum :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> AlbumItem -> AlbumDatagram
serializeAlbum artists albums allSongs (AlbumItem albumId albumPrice) =
  (album, maybeArtist, songs, albumPrice)
  where album = albums M.! albumId
        songs = filter (\x -> S.albumId x == albumId) (M.elems allSongs)
        maybeArtist = safeHead $ catMaybes $ (\x -> M.lookup (S.artistId x) artists) <$> songs -- refactor to monads