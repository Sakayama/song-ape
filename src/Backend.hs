-- each function represents a special http request type and route
module Backend where

import Order
import qualified Db

postOrder :: [ItemId] -> Maybe OrderDatagram
postOrder ids = (serializeOrder Db.artists Db.albums Db.songs) <$> createOrder Db.songPrices Db.albumPrices Db.orders ids

getOrders :: [OrderDatagram]
getOrders = fmap (serializeOrder Db.artists Db.albums Db.songs) Db.orders