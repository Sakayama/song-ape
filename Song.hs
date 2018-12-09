module Song (
  duration,
  Song,
  showSong
  ) where

import Data.Time.Clock

duration :: (Integer, Integer) -> DiffTime
duration (min, sec) = secondsToDiffTime $ min * 60 + sec

-- id, title, duration
type Song = (String, String, DiffTime)

showSong :: Song -> String
showSong (_, title, duration) = title ++ " " ++ show duration