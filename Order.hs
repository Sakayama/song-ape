module Order (Order, OrderContract, createOrder, serializeOrder) where

import Song
import Artist
import Data.List

-- id, price
type Order = [(String, Float)]

-- song, artist, price
type OrderContract = [(Song, Maybe Artist, Float)] 

-- takes a list of ids, a list of songs and a list of prices
-- returns an order in case if it's not empty
-- absence of data for any specific song does not lead to failure, instead it will be skipped
createOrder :: [String] -> [Song] -> [(String, Float)] -> Maybe Order
createOrder ids songs prices = 
  if null songsAndPrices then Nothing else Just songsAndPrices
  where songsAndPrices = [(id, price) | (songId, _, _, _) <- songs, id <- nub ids, (pId, price) <- prices, songId == id, pId == id]

-- prepares order to be transmitted to frontend
serializeOrder :: [Artist] -> [Song] -> Order -> OrderContract
serializeOrder artists songs order = 
  [(song, find (\(x, _) -> x == artistId) artists, price) | song@(songId, artistId, _, _) <- songs, (id, price) <- order, songId == id]