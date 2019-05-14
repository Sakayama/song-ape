module Frontend
  ( mainAction
  ) where

import qualified Album      (Album (..))
import qualified Artist     (Artist (..))

-- we should import only types, not specific functions
import qualified Backend
import           Data.List
import           Data.Maybe
import           Order      (ItemDatagram (..), ItemId (..), OrderDatagram (..))
import qualified Song       as S

mainAction :: String
mainAction = validOrderAction

validOrderAction =
  placeOrder [AlbumId "album1", AlbumId "album2", SongId "7", SongId "1a", SongId "a", SongId "z", SongId "1"]

repeatingIdsAction = placeOrder [SongId "7", SongId "7", SongId "7", AlbumId "album1", AlbumId "album1"]

invalidAction = placeOrder [SongId "z", AlbumId "album5"]

noIdsAction = placeOrder []

myOrdersAction = intercalate "\n\n" $ fmap showOrder Backend.getOrders

placeOrder :: [ItemId] -> String
placeOrder ids = fromMaybe errorMessage $ showOrder <$> Backend.postOrder ids
  where
    errorMessage = "Sorry\nwe were unable to create an order for you"

-- displays order as a string
showOrder :: OrderDatagram -> String
showOrder (OrderDatagram orderId items orderStatus) = title ++ displayedItems ++ total
  where
    title = "Order: #" ++ show orderId ++ "\n\n"
    displayedItems = intercalate "\n\n" $ showItem <$> items
    total = "\n\nTotal price: " ++ (show . sum . fmap itemPrice) items
    itemPrice (SongDatagram _ _ _ price)  = price
    itemPrice (AlbumDatagram _ _ _ price) = price

-- displays item as a string
showItem :: ItemDatagram -> String
showItem (AlbumDatagram album mArtist items price) = title ++ songs ++ total
  where
    title =
      "Album: " ++
      (Album.albumTitle album) ++ " (" ++ (show $ Album.albumYear album) ++ ")" ++ showArtist mArtist ++ "\n"
    showArtist Nothing  = ""
    showArtist (Just x) = ", " ++ (Artist.artistName x)
    songs = unlines $ (("- " ++) . showSong Nothing Nothing) <$> items
    total = "Album price: " ++ show price
showItem (SongDatagram song mArtist mAlbum _) = showSong mArtist mAlbum song

-- displays a song as a string
showSong :: Maybe Artist.Artist -> Maybe Album.Album -> S.Song -> String
showSong mArtist mAlbum song = artist ++ (S.title song) ++ album ++ " " ++ show (S.duration song)
  where
    artist =
      case mArtist of
        Nothing -> ""
        Just x  -> (Artist.artistName x) ++ " - "
    album =
      case mAlbum of
        Nothing -> ""
        Just x  -> " (" ++ (Album.albumTitle x) ++ ")"
