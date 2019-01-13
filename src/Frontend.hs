module Frontend (mainAction) where

-- we should import only types, not specific functions
import qualified Backend
import qualified Song as S
import qualified Artist (Artist(..))
import qualified Album (Album(..))
import Order (OrderDatagram, AlbumDatagram)
import Data.Maybe
import Data.List

mainAction :: String
mainAction = purchaseAlbumsAction

placeOrderAction = fromMaybe placeOrderMessage $ fmap showOrder $ Backend.postOrder ["7", "1a", "a", "z", "1"]
noIdsAction = fromMaybe placeOrderMessage $ fmap showOrder $ Backend.postOrder []
invalidIdsAction = fromMaybe placeOrderMessage $ fmap showOrder $ Backend.postOrder ["z"]
repeatingIdsAction = fromMaybe placeOrderMessage $ fmap showOrder $ Backend.postOrder ["7", "7", "7"]

purchaseAlbumsAction = fromMaybe placeOrderMessage $ (intercalate "\n\n" . fmap showAlbum) <$> Backend.purchaseAlbums ["album1", "album2", "album3", "album4"]
purchaseAlbumsAction2 = fromMaybe placeOrderMessage $ (intercalate "\n\n" . fmap showAlbum) <$> Backend.purchaseAlbums ["album5"]
purchaseAlbumsAction3 = fromMaybe placeOrderMessage $ (intercalate "\n\n" . fmap showAlbum) <$> Backend.purchaseAlbums []

myOrdersAction = intercalate "\n\n" $ fmap showOrder Backend.getOrders

-- error messages
placeOrderMessage = "Sorry\nwe were unable to create an order for you"

-- displays order as a string
showOrder :: OrderDatagram -> String
showOrder (orderId, items) =
  title ++ songs ++ total where
  title = "Order: #" ++ show orderId ++ "\n"
  songs = unlines $ (\(song, mArtist, mAlbum, _) -> showSong mArtist mAlbum song) <$> items
  total = "Total price: " ++ (show . sum . fmap (\(_, _, _, price) -> price)) items

showAlbum :: AlbumDatagram -> String
showAlbum (album, mArtist, items, price) =
  title ++ songs ++ total where
  title = "Album: " ++ (Album.albumTitle album) ++ " (" ++ (show $ Album.albumYear album) ++ ")" ++ showArtist mArtist ++ "\n"
  showArtist Nothing = ""
  showArtist (Just x) = ", " ++ (Artist.artistName x)
  songs = unlines $ (("- " ++) . showSong Nothing Nothing) <$> items
  total = "Album price: " ++ show price

-- displays a song as a string
showSong :: Maybe Artist.Artist -> Maybe Album.Album -> S.Song -> String
showSong mArtist mAlbum song = 
  artist ++ (S.title song) ++ album ++ " " ++ show (S.duration song) where 
    artist = case mArtist of Nothing -> ""
                             Just x -> (Artist.artistName x) ++ " - "
    album = case mAlbum of Nothing -> ""
                           Just x -> " (" ++ (Album.albumTitle x) ++ ")"