-- each function represents a special http request type and route
module Backend (postOrder) where

import Order
import qualified Db

postOrder :: [String] -> Maybe OrderContract
postOrder ids = fmap (serializeOrder Db.artists Db.songs) $ createOrder ids Db.songs Db.prices