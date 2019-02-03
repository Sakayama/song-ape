module Frontend (mainAction) where

-- we should import only types, not specific functions
import qualified Backend
import qualified Song as S
import qualified Artist (Artist(..))
import qualified Album (Album(..))
import Order (OrderDatagram(..), ItemDatagram(..), ItemId(..))
import Data.Maybe
import Data.List

mainAction :: String
mainAction = placeOrderAction

placeOrderAction = fromMaybe placeOrderMessage $ showSongOrder <$> Backend.postOrder [SongId "7", SongId "1a", SongId "a", SongId "z", SongId "1"]
noIdsAction = fromMaybe placeOrderMessage $ showSongOrder <$> Backend.postOrder []
invalidIdsAction = fromMaybe placeOrderMessage $ showSongOrder <$> Backend.postOrder [SongId "z"]
repeatingIdsAction = fromMaybe placeOrderMessage $ showSongOrder <$> Backend.postOrder [SongId "7", SongId "7", SongId "7"]

purchaseAlbumsAction = fromMaybe placeOrderMessage $ showAlbumOrder <$> Backend.purchaseAlbums [AlbumId "album1", AlbumId "album2", AlbumId "album3", AlbumId "album4"]
purchaseAlbumsAction2 = fromMaybe placeOrderMessage $ showAlbumOrder <$> Backend.purchaseAlbums [AlbumId "album5"]
purchaseAlbumsAction3 = fromMaybe placeOrderMessage $ showAlbumOrder <$> Backend.purchaseAlbums []

myOrdersAction = intercalate "\n\n" $ fmap showSongOrder Backend.getOrders

-- error messages
placeOrderMessage = "Sorry\nwe were unable to create an order for you"

-- displays order as a string
showSongOrder :: OrderDatagram -> String
showSongOrder (OrderDatagram orderId items) =
  title ++ songs ++ total where
  title = "Order: #" ++ show orderId ++ "\n"
  songs = unlines $ (\(SongDatagram song mArtist mAlbum _) -> showSong mArtist mAlbum song) <$> items
  total = "Total price: " ++ (show . sum . fmap (\(SongDatagram _ _ _ price) -> price)) items

showAlbumOrder :: OrderDatagram -> String
showAlbumOrder (OrderDatagram orderId items) =
  title ++ albums ++ total where
  title = "Order: #" ++ show orderId ++ "\n"
  albums = intercalate "\n\n" $ showAlbum <$> items
  total = "\nTotal price: " ++ (show . sum . fmap (\(AlbumDatagram _ _ _ price) -> price)) items

showAlbum :: ItemDatagram -> String
showAlbum (AlbumDatagram album mArtist items price) =
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