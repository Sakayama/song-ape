module Order where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Song
import Artist
import Album
import Util

-- orderId, [(songId, price)]
type Order = (Int, [(String, Float)])

-- orderId, [(song, artist, album, price)]
type OrderContract = (Int, [(Song, Maybe Artist, Maybe Album, Float)])

-- takes a list of prices, a list of previous orders and a list of ids
-- returns an order in case if it's not empty
-- absence of data for any specific song does not lead to failure, instead it will be skipped
-- we suppose that if we have price, then we'll definitely have a song
createOrder :: [(String, Float)] -> [Order] -> [String] -> Maybe Order
createOrder prices previousOrders ids = 
  if null orderContent then Nothing else Just (orderId, orderContent)
  where orderContent = catMaybes $ findPrice <$> nub ids
        findPrice id = find (\(x, _) -> x == id) prices
        orderId = (fromMaybe 0 . fmap succ . safeMaximum . fmap fst) previousOrders

-- prepares order to be transmitted to frontend
serializeOrder :: M.Map String Artist -> M.Map String Album -> M.Map String Song -> Order -> OrderContract
serializeOrder artists albums songs (orderId, orderContent) = (orderId, serializedContent) where
  serializedContent = [
    (song, M.lookup artistId artists, M.lookup albumId albums, price) |
    song@(songId, artistId, albumId, _, _) <- M.elems songs, 
    (id, price) <- orderContent, 
    songId == id
    ]
