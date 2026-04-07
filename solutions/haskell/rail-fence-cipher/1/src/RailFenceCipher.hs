module RailFenceCipher (encode, decode) where

railPattern :: Int -> [Int]
railPattern n = cycle ([0..n-1] ++ [n-2,n-3..1])

splitByCounts :: [Int] -> String -> [String]
splitByCounts [] _ = []
splitByCounts (c:cs) xs =
  let (front, rest) = splitAt c xs
  in front : splitByCounts cs rest

rebuild :: [Int] -> [String] -> String
rebuild [] _ = []
rebuild (r:rs) rails =
  let (c:cs) = rails !! r
      newRails = take r rails ++ [cs] ++ drop (r+1) rails
  in c : rebuild rs newRails

encode :: Int -> String -> String
encode n xs =
  let pattern = railPattern n
      paired = zip pattern xs
      pick r = [c | (rail, c) <- paired, rail == r]
  in concat [pick r | r <- [0..n-1]]

decode :: Int -> String -> String
decode n xs
  | n <= 1 || n >= length xs = xs
  | otherwise =
      let pattern = take (length xs) (railPattern n)
          counts = [length (filter (== r) pattern) | r <- [0..n-1]]
          rails = splitByCounts counts xs
      in rebuild pattern rails
      
