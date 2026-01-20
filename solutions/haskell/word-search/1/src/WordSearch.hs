module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe

data Direction = DirUp | DirDown | DirLeft | DirRight | DiagUL | DiagUR | DiagDL | DiagDR deriving (Enum)
data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

dirDelta :: Direction -> (Int, Int)
dirDelta DirUp     = ( 0, -1)
dirDelta DirDown   = ( 0,  1)
dirDelta DirLeft   = (-1,  0)
dirDelta DirRight  = ( 1,  0)
dirDelta DiagUL = (-1, -1)
dirDelta DiagUR = ( 1, -1)
dirDelta DiagDL = (-1,  1)
dirDelta DiagDR = ( 1,  1)


isEndOfGrid :: [String] -> CharPos -> Bool
isEndOfGrid grid (CharPos col row) = (row == (length grid)) && (col == (length $ head grid))

nextCharPos :: [String] -> CharPos -> CharPos
nextCharPos grid (CharPos col row)
    | col == length (head grid) = (CharPos 1 (row+1))
    | otherwise = (CharPos (col+1) row)

getWordPos :: String -> CharPos -> Direction -> WordPos
getWordPos word (CharPos col row) dir =
  WordPos
    (CharPos col row)
    (CharPos (col + x * (len - 1)) (row + y * (len - 1)))
  where
    (x, y) = dirDelta dir
    len    = length word

getNewCharPos :: Direction -> CharPos -> CharPos
getNewCharPos dir (CharPos c r) =
  let (dc, dr) = dirDelta dir
  in CharPos (c + dc) (r + dr)

getCharAtPosition :: [String] -> CharPos -> Maybe Char
getCharAtPosition grid (CharPos col row)
    | row > length grid || col > length (head grid) || row < 1 || col < 1 = Nothing
    | otherwise = Just (grid !! (row - 1) !! (col - 1))

checkWordAlongDirection :: [String] -> String -> CharPos -> Direction -> Bool
checkWordAlongDirection grid "" _ _ = True
checkWordAlongDirection grid word charpos dir
    | isJust (getCharAtPosition grid charpos) && (fromJust (getCharAtPosition grid charpos) == (head word)) = True && checkWordAlongDirection grid (tail word) (getNewCharPos dir charpos) dir
    | otherwise = False

searchWordAlongDirectionFromIndex :: [String] -> String -> CharPos -> Direction -> [(String, Maybe WordPos)]
searchWordAlongDirectionFromIndex grid word charpos dir
  | checkWordAlongDirection grid word charpos dir = [(word, Just (getWordPos word charpos dir))]
  | otherwise = []

searchWordAlongDirection :: [String] -> String -> CharPos -> Direction -> [(String, Maybe WordPos)]
searchWordAlongDirection grid word charpos dir
  | isEndOfGrid grid charpos = searchWordAlongDirectionFromIndex grid word charpos dir
  | otherwise = (searchWordAlongDirectionFromIndex grid word charpos dir) ++ (searchWordAlongDirection grid word (nextCharPos grid charpos) dir)

searchWordAlongPossibleDirections :: [String] -> String -> CharPos -> [(String, Maybe WordPos)]
searchWordAlongPossibleDirections grid word charpos = 
  searchFunction DirLeft ++ 
  searchFunction DirRight ++
  searchFunction DirUp ++ 
  searchFunction DirDown ++
  searchFunction DiagUL ++
  searchFunction DiagUR ++
  searchFunction DiagDL ++
  searchFunction DiagDR
  where searchFunction = searchWordAlongDirection grid word charpos

searchWord :: [String] -> String -> [(String, Maybe WordPos)]
searchWord grid word
  | length (searchWordAlongPossibleDirections grid word (CharPos 1 1)) == 0 = [(word, Nothing)]
  | otherwise = searchWordAlongPossibleDirections grid word (CharPos 1 1)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid (word:[]) = searchWord grid word
search grid (word:wordList) = searchWord grid word ++ search grid wordList
