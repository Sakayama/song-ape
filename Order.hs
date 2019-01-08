module Order (Order(..), OrderContract, OrderedAlbum, OrderedAlbumContract, createOrder, orderOnlyAlbums, serializeOrder, serializeAlbum) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Song as S
import Artist
import Album
import Util

-- content :: [(songId, price)]
data Order = Order { orderId :: Int, content :: [(String, Float)] }

-- orderId, [(song, artist, album, price)]
type OrderContract = (Int, [(S.Song, Maybe Artist, Maybe Album, Float)])

-- albumId, albumPrice
type OrderedAlbum = (String, Float)

type OrderedAlbumContract = (Album, Maybe Artist, [S.Song], Float)

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

orderOnlyAlbums :: [(String, Float)] -> [String] -> Maybe [OrderedAlbum]
orderOnlyAlbums albumPrices ids = 
  if null content then Nothing else Just content
  where content = catMaybes $ findPrice <$> nub ids
        findPrice id = find (\(x, _) -> x == id) albumPrices

-- prepares order to be transmitted to frontend
serializeOrder :: M.Map String Artist -> M.Map String Album -> M.Map String S.Song -> Order -> OrderContract
serializeOrder artists albums songs (Order orderId orderContent) = (orderId, serializedContent) where
  serializedContent = [
    (song, M.lookup (S.artistId song) artists, M.lookup (S.albumId song) albums, price) |
    song <- M.elems songs, 
    (id, price) <- orderContent, 
    S.songId song == id
    ]

serializeAlbum :: M.Map String Artist -> M.Map String Album -> M.Map String S.Song -> OrderedAlbum -> OrderedAlbumContract
serializeAlbum artists albums allSongs (albumId, albumPrice) =
  (album, maybeArtist, songs, albumPrice)
  where album = albums M.! albumId
        songs = filter (\x -> S.albumId x == albumId) (M.elems allSongs)
        maybeArtist = safeHead $ catMaybes $ (\x -> M.lookup (S.artistId x) artists) <$> songs -- refactor to monads