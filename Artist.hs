module Artist (Artist(..)) where

data Artist = Artist { artistId :: String, 
                       artistName :: String
                       } deriving (Show, Ord, Eq)