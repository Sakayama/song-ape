module Song (Song(..), diffTime) where

import Data.Time.Clock
import Data.List

-- TODO: keeping a [artistId] instead of artistId provides flexibility
data Song = Song { songId :: String
                 , artistId :: String
                 , albumId :: String
                 , title :: String
                 , duration :: DiffTime
                 }

-- crutch
diffTime :: (Integer, Integer) -> DiffTime
diffTime (min, sec) = secondsToDiffTime $ min * 60 + sec