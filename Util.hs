module Util where

safeMaximum :: (Ord a, Foldable f) => f a -> Maybe a
safeMaximum xs
  | null xs   = Nothing
  | otherwise = Just $ maximum xs