module Album (Album(..)) where

import Order

data Album = 
    Album { albumId :: String, 
    albumTitle :: String, 
    albumYear :: Int,
    albumCover :: String,
    albimDescription :: String 
    } deriving (Show, Eq, Ord)

