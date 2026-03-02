module ProteinTranslation(proteins) where

translationTable :: String -> Maybe String
translationTable "UAA" = Just "STOP"
translationTable "UAG" = Just "STOP"
translationTable "UGA" = Just "STOP"
translationTable [] = Just "STOP"
translationTable "AUG" = Just "Methionine"
translationTable "UUU" = Just "Phenylalanine"
translationTable "UUC" = Just "Phenylalanine"
translationTable "UUA" = Just "Leucine"
translationTable "UUG" = Just "Leucine"
translationTable ('U':'C':_) = Just "Serine"
translationTable ('U':'A':_) = Just "Tyrosine"
translationTable "UGG" = Just "Tryptophan"
translationTable ('U':'G':_) = Just "Cysteine"
translationTable _ = Nothing

proteins :: String -> Maybe [String]
proteins (x:y:z:xs) = 
  case translationTable (x:y:z:[]) of
    Nothing -> Nothing
    (Just "STOP") -> Just []
    (Just protein) -> 
      case proteins xs of
        Nothing -> Nothing
        (Just proteinList) -> (Just (protein:proteinList))
    
    

proteins _ = Just []
  
