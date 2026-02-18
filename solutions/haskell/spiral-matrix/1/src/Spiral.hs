module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n = [[val r c | c <- [0..n-1]] | r <- [0..n-1]]
  where
    val r c = spiralVal n 0 0 (n-1) (n-1) r c 1
    spiralVal n minR minC maxR maxC r c k
      | r == minR = k + (c - minC)
      | c == maxC = k + sideLength + (r - minR)
      | r == maxR = k + 2 * sideLength + (maxC - c)
      | c == minC = k + 3 * sideLength + (maxR - r)
      | otherwise = spiralVal (n-2) (minR+1) (minC+1) (maxR-1) (maxC-1) r c (k + 4 * sideLength)
        where sideLength = maxR - minR
