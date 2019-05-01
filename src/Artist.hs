module Artist (Artist(..)) where

data Artist = Artist { artistId :: String, 
                       artistName :: String,
                       artistPhoto :: String,
                       artistDescription :: String
                       } deriving (Show, Ord, Eq)