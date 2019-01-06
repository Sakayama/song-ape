-- we should import only types, not specific functions
import qualified Backend
import Song (Song)
import Artist (Artist(..))
import Album (Album)
import Order (OrderContract, OrderedAlbumContract)
import Data.Maybe
import Data.List

main = putStrLn purchaseAlbumsAction

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
showOrder :: OrderContract -> String
showOrder (orderId, items) =
  title ++ songs ++ total where
  title = "Order: #" ++ show orderId ++ "\n"
  songs = unlines $ (\(song, mArtist, mAlbum, _) -> showSong mArtist mAlbum song) <$> items
  total = "Total price: " ++ (show . sum . fmap (\(_, _, _, price) -> price)) items

showAlbum :: OrderedAlbumContract -> String
showAlbum ((_, albumTitle), mArtist, items, price) =
  title ++ songs ++ total where
  title = "Album: " ++ albumTitle ++ showArtist mArtist ++ "\n"
  showArtist Nothing = ""
  showArtist (Just (Artist _ x)) = ", " ++ x
  songs = unlines $ (("- " ++) . showSong Nothing Nothing) <$> items
  total = "Album price: " ++ show price

-- displays a song as a string
showSong :: Maybe Artist -> Maybe Album -> Song -> String
showSong mArtist mAlbum (_, _, _, title, duration) = 
  artistName ++ title ++ albumTitle ++ " " ++ show duration
  where artistName = case mArtist of Nothing -> ""
                                     Just (Artist _ artistName) -> artistName ++ " - "
        albumTitle = case mAlbum of Nothing -> ""
                                    Just (_, albumTitle) -> " (" ++ albumTitle ++ ")"