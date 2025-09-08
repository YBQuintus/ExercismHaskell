module DNA (toRNA) where

import Data.Maybe (fromMaybe)

toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA (x:xs) =
  case toRNATable x of
    Just r  -> case toRNA xs of
                 Right rs -> Right (r : rs)
                 Left err -> Left err
    Nothing -> Left x


toRNATable :: Char -> Maybe Char
toRNATable 'G' = (Just 'C')
toRNATable 'C' = (Just 'G')
toRNATable 'T' = (Just 'A')
toRNATable 'A' = (Just 'U')
toRNATable _ = Nothing