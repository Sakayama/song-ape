-- each function represents a special http request type and route
module Backend where

import Order
import qualified Db

postOrder :: [SongId] -> Maybe OrderDatagram
postOrder ids = fmap (serializeOrder Db.artists Db.albums Db.songs) $ createOrder Db.songPrices Db.orders ids

purchaseAlbums :: [AlbumId] -> Maybe [AlbumDatagram]
purchaseAlbums ids = (serializeAlbum Db.artists Db.albums Db.songs <$>) <$> (orderOnlyAlbums Db.albumPrices ids)

getOrders :: [OrderDatagram]
getOrders = fmap (serializeOrder Db.artists Db.albums Db.songs) Db.orders