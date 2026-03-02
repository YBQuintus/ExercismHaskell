module Frequency (frequency) where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Char (isAlpha, toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 1 = countSlice 0 len
  | otherwise     = Map.unionsWith (+) partials
  where
    vec = V.fromList texts
    len = V.length vec

    chunkSize = (len + nWorkers - 1) `div` nWorkers

    slices =
      [ (start, min len (start + chunkSize))
      | start <- takeWhile (< len) [0, chunkSize ..]
      ]

    partials = parMap rdeepseq (uncurry countSlice) slices

    countSlice start end =
      V.foldl' countText Map.empty
        (V.slice start (end - start) vec)

countText :: Map Char Int -> Text -> Map Char Int
countText acc txt =
  T.foldl'
    (\m c ->
        if isAlpha c
          then Map.insertWith (+) (toLower c) 1 m
          else m)
    acc
    txt