module Song (
  duration,
  Song
  ) where

import Data.Time.Clock
import Data.List
import Artist

-- id, artistId, title, duration
type Song = (String, String, String, DiffTime)

-- crutch
duration :: (Integer, Integer) -> DiffTime
duration (min, sec) = secondsToDiffTime $ min * 60 + sec