module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

toInternal :: Coord -> Coord
toInternal (x, y) = (x - 1, y - 1)

toExternal :: Coord -> Coord
toExternal (x, y) = (x + 1, y + 1)

convertSetOut :: Set Coord -> Set Coord
convertSetOut = Set.map toExternal

getNeighbours :: Coord -> [Coord]
getNeighbours (x, y) =
  [ (x-1, y)
  , (x+1, y)
  , (x, y-1)
  , (x, y+1)
  ]

getCell :: [String] -> Coord -> Maybe Char
getCell board (x, y)
  | y < 0 || y >= length board = Nothing
  | x < 0 || x >= length (board !! y) = Nothing
  | otherwise = Just ((board !! y) !! x)

getOwner :: [String] -> Coord -> Maybe Color
getOwner board coord =
  case getCell board coord of
    Just 'B' -> Just Black
    Just 'W' -> Just White
    _        -> Nothing

isEmpty :: [String] -> Coord -> Bool
isEmpty board coord =
  case getCell board coord of
    Just ' ' -> True
    _        -> False

floodFill :: [String] -> Set Coord -> Coord -> (Set Coord, Set Color)
floodFill board visited start
  | Set.member start visited = (visited, Set.empty)
  | not (isEmpty board start) = (visited, Set.empty)
  | otherwise =
      go (Set.insert start visited) Set.empty (getNeighbours start)
  where
    go terr borders [] = (terr, borders)

    go terr borders (c:cs)
      | Set.member c terr =
          go terr borders cs

      | isEmpty board c =
          let (terr', borders') = floodFill board terr c
          in go terr' (Set.union borders borders') cs

      | otherwise =
          case getOwner board c of
            Just col -> go terr (Set.insert col borders) cs
            Nothing  -> go terr borders cs

territoryOwner :: Set Color -> Maybe Color
territoryOwner borders =
  case Set.toList borders of
    [c] -> Just c
    _   -> Nothing

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
  let internal = toInternal coord
  in if not (isEmpty board internal)
        then Nothing
        else
          let (terr, borders) = floodFill board Set.empty internal
          in Just (convertSetOut terr, territoryOwner borders)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
  go Set.empty allCoords
  where
    height = length board
    width  = if null board then 0 else length (head board)

    allCoords =
      [ (x, y)
      | y <- [0 .. height - 1]
      , x <- [0 .. width - 1]
      ]

    go _ [] = []

    go visited (c:cs)
      | Set.member c visited = go visited cs
      | not (isEmpty board c) = go visited cs
      | otherwise =
          case territoryFor board (toExternal c) of
            Just (terr, owner) ->
              let internalTerr = Set.map toInternal terr
              in (terr, owner) : go (Set.union visited internalTerr) cs
            Nothing ->
              go visited cs
