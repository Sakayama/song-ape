module Order (placeOrder, Order) where

import Song

placeOrder :: [String] -> [Song] -> [(String, Float)] -> Maybe Order
placeOrder ids songs prices = 
  if null songsAndPrices then Nothing else Just (map fst songsAndPrices, (sum . map snd) songsAndPrices)
  where songsAndPrices = [(song, price) | song@(songId, _, _) <- songs, id <- ids, (pId, price) <- prices, songId == id, pId == id]

-- items, total price
type Order = ([Song], Float)
