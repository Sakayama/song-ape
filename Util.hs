module Util where

safeMaximum :: (Ord a, Foldable f) => f a -> Maybe a
safeMaximum xs
  | null xs   = Nothing
  | otherwise = Just $ maximum xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x