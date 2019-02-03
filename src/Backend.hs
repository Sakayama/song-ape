-- each function represents a special http request type and route
module Backend where

import Order
import qualified Db

postOrder :: [ItemId] -> Maybe OrderDatagram
postOrder ids = (serializeSongOrder Db.artists Db.albums Db.songs) <$> createSongOrder Db.songPrices Db.albumPrices Db.orders ids

purchaseAlbums :: [ItemId] -> Maybe OrderDatagram
purchaseAlbums ids = (serializeAlbumOrder Db.artists Db.albums Db.songs) <$> createAlbumOrder Db.songPrices Db.albumPrices Db.orders ids

getOrders :: [OrderDatagram]
getOrders = fmap (serializeSongOrder Db.artists Db.albums Db.songs) Db.orders