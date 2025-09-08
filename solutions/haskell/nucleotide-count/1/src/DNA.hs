module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldr countChar (Right Map.empty)
  where 
    countChar :: Char -> Either String (Map Nucleotide Int) -> Either String (Map Nucleotide Int)
    countChar _ (Left err) = Left err
    countChar c (Right m) =
      case charToNucleotide c of
        Just n -> Right (Map.insertWith (+) n 1 m)
        Nothing -> Left ("Invalid nucleotide " ++ [c])



charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide 'A' = Just A
charToNucleotide 'C' = Just C
charToNucleotide 'G' = Just G
charToNucleotide 'T' = Just T
charToNucleotide _   = Nothing
