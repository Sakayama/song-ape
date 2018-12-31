module Util where

maximumMaybe :: (Ord a, Foldable f) => f a -> Maybe a
maximumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $ maximum xs