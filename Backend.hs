-- each function represents a special http request type and route
module Backend where

import Order
import Album -- remove me
import qualified Db

postOrder :: [String] -> Maybe OrderContract
postOrder ids = fmap (serializeOrder Db.artists Db.albums Db.songs) $ createOrder Db.songPrices Db.orders ids

purchaseAlbums :: [String] -> Maybe [OrderedAlbumContract]
purchaseAlbums ids = (serializeAlbum Db.artists Db.albums Db.songs <$>) <$> (orderOnlyAlbums Db.albumPrices ids)

getOrders :: [OrderContract]
getOrders = fmap (serializeOrder Db.artists Db.albums Db.songs) Db.orders