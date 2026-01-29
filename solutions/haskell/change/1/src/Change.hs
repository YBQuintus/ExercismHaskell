module Change (findFewestCoins) where

import Data.Maybe
import Data.List
import Data.Ord (comparing)
import qualified Data.Map.Strict as M

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = go target
  where
    validCoins = filter (> 0) coins
    go x
      | x < 0 = Nothing
      | otherwise = memoized x
    memoized = (map go' [0..] !!) . fromIntegral

    go' 0 = Just []
    go' t
      | t < 0     = Nothing
      | otherwise =
          minimumByLength $
            mapMaybe (\c -> (c :) <$> go (t - c)) $
              filter (<= t) validCoins

    minimumByLength [] = Nothing
    minimumByLength xs = Just $ minimumBy (comparing length) xs
