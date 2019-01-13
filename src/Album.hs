module Album (Album(..)) where

data Album = Album { albumId :: String, albumTitle :: String, albumYear :: Int } deriving (Show, Eq, Ord)

