-- we should import only types, not specific functions
import qualified Backend
import Song (Song)
import Artist (Artist)
import Order (OrderContract)

main = putStrLn repeatingIdsAction

placeOrderAction = showOrder $ Backend.postOrder ["7", "1a", "a", "z"]
noIdsAction = showOrder $ Backend.postOrder []
invalidIdsAction = showOrder $ Backend.postOrder ["z"]
repeatingIdsAction = showOrder $ Backend.postOrder ["7", "7", "7"]

-- displays order as a string
showOrder :: Maybe OrderContract -> String
showOrder Nothing = "Sorreeey"
showOrder (Just items) =
  title ++ songs ++ total where
  title = "Order: #" ++ "\n"
  songs = unlines $ map (\(song, mArtist, _) -> showSong mArtist song) items
  total = "Total price: " ++ (show . sum . map (\(_, _, price) -> price)) items

-- displays a song as a string
showSong :: Maybe Artist -> Song -> String
showSong mArtist (_, _, title, duration) = 
  artistName ++ title ++ " " ++ show duration
  where artistName = case mArtist of Nothing -> ""
                                     (Just (_, artistName)) -> artistName ++ " - "