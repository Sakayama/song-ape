module Song where

import Data.Time.Clock
import Data.List
import Artist

-- id, artistId, albumId, title, duration
type Song = (String, String, String, String, DiffTime)
-- keeping a [artistId] instead of artistId provides flexibility

-- crutch
duration :: (Integer, Integer) -> DiffTime
duration (min, sec) = secondsToDiffTime $ min * 60 + sec