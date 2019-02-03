module Order (
  Order(..) -- for Db
  , ItemId(..)
  , OrderItem(..) -- for Db
  , ItemDatagram(..)
  , OrderDatagram(..)
  , createSongOrder
  , createAlbumOrder
  , serializeSongOrder
  , serializeAlbum
  , serializeAlbumOrder) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Song as S
import qualified Artist
import qualified Album
import Util

data ItemId = SongId String 
  | AlbumId String deriving (Eq)

data OrderItem = SongItem { songId :: String, price :: Float } 
  | AlbumItem { albumId :: String, albumPrice :: Float }

data Order = Order { orderId :: Int, orderContent :: [OrderItem] }

data ItemDatagram = SongDatagram S.Song (Maybe Artist.Artist) (Maybe Album.Album) Float
  | AlbumDatagram Album.Album (Maybe Artist.Artist) [S.Song] Float

data OrderDatagram = OrderDatagram Int [ItemDatagram]

-- takes a list of prices, a list of previous orders and a list of ids
-- returns an order in case if it's not empty
-- absence of data for any specific song does not lead to failure, instead it will be skipped
-- we suppose that if we have price, then we'll definitely have a song
createSongOrder :: [(String, Float)] -> [(String, Float)] -> [Order] -> [ItemId] -> Maybe Order
createSongOrder songPrices albumPrices previousOrders ids = 
  if null orderContent then Nothing else Just (Order nextId orderContent)
  where orderContent = fmap (\(a, b) -> SongItem a b) $ catMaybes $ findPrice <$> nub ids
        findPrice (SongId id) = find (\(x, _) -> x == id) songPrices
        nextId = (fromMaybe 0 . fmap succ . safeMaximum . fmap orderId) previousOrders

createAlbumOrder :: [(String, Float)] -> [(String, Float)] -> [Order] -> [ItemId] -> Maybe Order
createAlbumOrder songPrices albumPrices previousOrders ids = 
  if null items then Nothing else Just (Order 100500 items) -- refactor if + catMaybes to a specialized function
  where items = catMaybes $ findPrice <$> nub ids
        findPrice (AlbumId id) = (\(x, y) -> AlbumItem x y) <$> find (\(x, _) -> x == id) albumPrices

-- prepares order to be transmitted to frontend
serializeSongOrder :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> Order -> OrderDatagram
serializeSongOrder artists albums songs (Order orderId orderContent) = OrderDatagram orderId serializedContent 
  where serializedContent = (serializeSong artists albums songs) <$> orderContent

serializeAlbumOrder :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> Order -> OrderDatagram
serializeAlbumOrder artists albums songs (Order orderId orderContent) = OrderDatagram orderId serializedContent
  where serializedContent = (serializeAlbum artists albums songs) <$> orderContent 

serializeSong :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> OrderItem -> ItemDatagram
serializeSong artists albums allSongs (SongItem id price) = 
  SongDatagram song (M.lookup (S.artistId song) artists) (M.lookup (S.albumId song) albums) price
  where song = allSongs M.! id

serializeAlbum :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> OrderItem -> ItemDatagram
serializeAlbum artists albums allSongs (AlbumItem albumId albumPrice) =
  AlbumDatagram album maybeArtist songs albumPrice
  where album = albums M.! albumId
        songs = filter (\x -> S.albumId x == albumId) (M.elems allSongs)
        maybeArtist = safeHead $ catMaybes $ (\x -> M.lookup (S.artistId x) artists) <$> songs -- refactor to monads
