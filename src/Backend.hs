-- each function represents a special http request type and route
module Backend where

import Order
import qualified Db

postOrder :: [SongId] -> Maybe SongOrderDatagram
postOrder ids = (serializeSongOrder Db.artists Db.albums Db.songs) <$> createSongOrder Db.songPrices Db.albumPrices Db.orders ids

purchaseAlbums :: [AlbumId] -> Maybe AlbumOrderDatagram
purchaseAlbums ids = (serializeAlbumOrder Db.artists Db.albums Db.songs) <$> createAlbumOrder Db.songPrices Db.albumPrices Db.orders ids

getOrders :: [SongOrderDatagram]
getOrders = fmap (serializeSongOrder Db.artists Db.albums Db.songs) Db.orders