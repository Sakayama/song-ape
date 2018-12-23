module Order (Order, OrderContract, createOrder, serializeOrder) where

import Song
import Artist
import Album
import Data.List
import Data.Maybe

-- id, price
type Order = [(String, Float)]

-- song, artist, price
type OrderContract = [(Song, Maybe Artist, Maybe Album, Float)] 

-- takes a list of ids, a list of songs and a list of prices
-- returns an order in case if it's not empty
-- absence of data for any specific song does not lead to failure, instead it will be skipped
-- we suppose that if we have price, then we'll definitely have a song
createOrder :: [String] -> [(String, Float)] -> Maybe Order
createOrder ids prices = 
  if null songsAndPrices then Nothing else Just songsAndPrices
  where songsAndPrices = catMaybes $ map findPrice $ nub ids
        findPrice id = find (\(x, _) -> x == id) prices

-- prepares order to be transmitted to frontend
serializeOrder :: [Artist] -> [Album] -> [Song] -> Order -> OrderContract
serializeOrder artists albums songs order = 
  [(song, find (\(x, _) -> x == artistId) artists, find (\(x, _) -> x == albumId) albums, price) | song@(songId, artistId, albumId, _, _) <- songs, (id, price) <- order, songId == id]