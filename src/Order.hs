module Order (
  Order(..) -- for Db
  , ItemId(..)
  , OrderItem(..) -- for Db
  , ItemDatagram(..)
  , OrderDatagram(..)
  , OrderStatus(..)
  , createOrder
  , serializeOrder
  , serializeAlbum) where -- ?

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

data Order = Order { orderId :: Int, orderContent :: [OrderItem], orderStatus :: OrderStatus }

data OrderStatus = Unconfirmed | Confirmed Int

data ItemDatagram = SongDatagram S.Song (Maybe Artist.Artist) (Maybe Album.Album) Float
  | AlbumDatagram Album.Album (Maybe Artist.Artist) [S.Song] Float

data OrderDatagram = OrderDatagram Int [ItemDatagram] OrderStatus

-- takes two lists of prices, a list of previous orders and a list of ids
-- returns an order in case if it's not empty
-- absence of data for any specific item does not lead to failure, instead it will be skipped
-- we suppose that if we have price, then we'll definitely have a song
createOrder :: [(String, Float)] -> [(String, Float)] -> [Order] -> [ItemId] -> Maybe Order
createOrder songPrices albumPrices previousOrders ids = 
  if null items then Nothing else Just (Order nextId items (Confirmed 98098098))
  where items = catMaybes $ findPrice <$> nub ids
        findPrice (SongId id) = (\(a, b) -> SongItem a b) <$> find (\(x, _) -> x == id) songPrices
        findPrice (AlbumId id) = (\(x, y) -> AlbumItem x y) <$> find (\(x, _) -> x == id) albumPrices
        nextId = (fromMaybe 0 . fmap succ . safeMaximum . fmap orderId) previousOrders

-- prepares order to be transmitted to frontend
serializeOrder :: M.Map String Artist.Artist -> M.Map String Album.Album -> M.Map String S.Song -> Order -> OrderDatagram
serializeOrder artists albums songs (Order orderId items (Confirmed 98098098)) = OrderDatagram orderId serializedContent (Confirmed 98098098)
  where serializedContent = serializeItem <$> items
        serializeItem x@(AlbumItem _ _) = serializeAlbum artists albums songs x
        serializeItem x@(SongItem _ _) = serializeSong artists albums songs x

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
