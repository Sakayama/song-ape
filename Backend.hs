-- each function represents a special http request type and route
module Backend where

import Order
import qualified Db

postOrder :: [String] -> Maybe OrderContract
postOrder ids = fmap (serializeOrder Db.artists Db.albums Db.songs) $ createOrder Db.prices Db.orders ids

getOrders :: [OrderContract]
getOrders = fmap (serializeOrder Db.artists Db.albums Db.songs) Db.orders