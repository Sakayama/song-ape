-- we should import only types, not specific functions
import qualified Backend
import Song (Song)
import Artist (Artist)
import Album (Album)
import Order (OrderContract)
import Data.Maybe
import Data.List

main = putStrLn placeOrderAction

placeOrderAction = fromMaybe placeOrderMessage $ fmap showOrder $ Backend.postOrder ["7", "1a", "a", "z", "1"]
noIdsAction = fromMaybe placeOrderMessage $ fmap showOrder $ Backend.postOrder []
invalidIdsAction = fromMaybe placeOrderMessage $ fmap showOrder $ Backend.postOrder ["z"]
repeatingIdsAction = fromMaybe placeOrderMessage $ fmap showOrder $ Backend.postOrder ["7", "7", "7"]

myOrdersAction = intercalate "\n\n" $ map showOrder Backend.getOrders

-- error messages
placeOrderMessage = "Sorry\nwe were unable to create an order for you"

-- displays order as a string
showOrder :: OrderContract -> String
showOrder (orderId, items) =
  title ++ songs ++ total where
  title = "Order: #" ++ show orderId ++ "\n"
  songs = unlines $ map (\(song, mArtist, mAlbum, _) -> showSong mArtist mAlbum song) items
  total = "Total price: " ++ (show . sum . map (\(_, _, _, price) -> price)) items

-- displays a song as a string
showSong :: Maybe Artist -> Maybe Album -> Song -> String
showSong mArtist mAlbum (_, _, _, title, duration) = 
  artistName ++ title ++ albumTitle ++ " " ++ show duration
  where artistName = case mArtist of Nothing -> ""
                                     Just (_, artistName) -> artistName ++ " - "
        albumTitle = case mAlbum of Nothing -> ""
                                    Just (_, albumTitle) -> " (" ++ albumTitle ++ ")"