module Order (
  SongOrder(..)
  , SongId(..)
  , AlbumId(..)
  , SongItem(..)
  , SongDatagram(..)
  , SongOrderDatagram(..)
  , AlbumDatagram(..)
  , AlbumOrderDatagram(..)
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

-- data ItemId = SongId | AlbumId
data SongId = SongId String deriving (Eq)
data AlbumId = AlbumId String deriving (Eq)

-- data Item = SongItem | AlbumItem
data SongItem = SongItem { songId :: String, price :: Float }
data AlbumItem = AlbumItem { albumId :: String, albumPrice :: Float }

-- data Order = SongOrder | AlbumOrder
data SongOrder = SongOrder { orderId :: Int, songContent :: [SongItem] }
data AlbumOrder = AlbumOrder { oId :: Int, albumContent :: [AlbumItem] }

-- data ItemDatagram = SongDatagram | AlbumDatagram
-- (song, artist, album, price)
data SongDatagram = SongDatagram S.Song (Maybe Artist.Artist) (Maybe Album.Album) Float
data AlbumDatagram = AlbumDatagram Album.Album (Maybe Artist.Artist) [S.Song] Float

-- data OrderDatagram = SongOrderDatagram | AlbumOrderDatagram
data SongOrderDatagram = SongOrderDatagram Int [SongDatagram]
data AlbumOrderDatagram = AlbumOrderDatagram Int [AlbumDatagram]


-- takes a list of prices, a list of previous orders and a list of ids
-- returns an order in case if it's not empty
-- absence of data for any specific song does not lead to failure, instead it will be skipped
-- we suppose that if we have price, then we'll definitely have a song
createSongOrder :: [(String, Float)] -> [(String, Float)] -> [SongOrder] -> [SongId] -> Maybe SongOrder
createSongOrder songPrices albumPrices previousOrders ids = 
  if null orderContent then Nothing else Just (SongOrder nextId orderContent)
  where orderContent = fmap (\(a, b) -> SongItem a b) $ catMaybes $ findPrice <$> nub ids
        findPrice (SongId id) = find (\(x, _) -> x == id) songPrices
        nextId = (fromMaybe 0 . fmap succ . safeMaximum . fmap orderId) previousOrders

createAlbumOrder :: [(String, Float)] -> [(String, Float)] -> [SongOrder] -> [AlbumId] -> Maybe AlbumOrder
createAlbumOrder songPrices albumPrices previousOrders ids = 
  if null items then Nothing else Just (AlbumOrder 100500 items) -- refactor if + catMaybes to a specialized function
  where items = catMaybes $ findPrice <$> nub ids
        findPrice (AlbumId id) = (\(x, y) -> AlbumItem x y) <$> find (\(x, _) -> x == id) albumPrices

-- prepares order to be transmitted to frontend
serializeSongOrder :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> SongOrder -> SongOrderDatagram
serializeSongOrder artists albums songs (SongOrder orderId orderContent) = SongOrderDatagram orderId serializedContent 
  where serializedContent = (serializeSong artists albums songs) <$> orderContent

serializeAlbumOrder :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> AlbumOrder -> AlbumOrderDatagram
serializeAlbumOrder artists albums songs (AlbumOrder orderId orderContent) = AlbumOrderDatagram orderId serializedContent
  where serializedContent = (serializeAlbum artists albums songs) <$> orderContent 

serializeSong :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> SongItem -> SongDatagram
serializeSong artists albums allSongs (SongItem id price) = 
  SongDatagram song (M.lookup (S.artistId song) artists) (M.lookup (S.albumId song) albums) price
  where song = allSongs M.! id

serializeAlbum :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> AlbumItem -> AlbumDatagram
serializeAlbum artists albums allSongs (AlbumItem albumId albumPrice) =
  AlbumDatagram album maybeArtist songs albumPrice
  where album = albums M.! albumId
        songs = filter (\x -> S.albumId x == albumId) (M.elems allSongs)
        maybeArtist = safeHead $ catMaybes $ (\x -> M.lookup (S.artistId x) artists) <$> songs -- refactor to monads
