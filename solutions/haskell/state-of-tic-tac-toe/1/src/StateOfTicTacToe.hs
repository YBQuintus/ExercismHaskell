module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not (validGame board) || ((win board 'X') && (win board 'O'))= Impossible
  | win board 'X' = WinX
  | win board 'O' = WinO
  | filled board = Draw
  | otherwise = Ongoing

countXOs :: [String] -> (Int, Int)
countXOs xss = (x, o)
  where
    x = sum $ map (length . filter (== 'X')) xss
    o = sum $ map (length . filter (== 'O')) xss

correctSize :: [String] -> Bool
correctSize xss = length xss == 3 && all ((== 3) . length) xss

validGame :: [String] -> Bool
validGame xss = correctSize xss && (x - o == 1) || (x - o == 0)
  where
    (x, o) = countXOs xss

filled :: [String] -> Bool
filled xss = all (all (/=' ')) xss

win :: [String] -> Char -> Bool
win board c =
  any (== replicate 3 c) board ||

  any (== replicate 3 c) (transpose board) ||

  [board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2] == replicate 3 c ||
  [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0] == replicate 3 c
  where
    transpose :: [[a]] -> [[a]]
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)

  

    
