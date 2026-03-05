module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

toDecimal :: Integral a => a -> [a] -> Either (Error a) a
toDecimal base digits = foldl step (Right 0) digits
  where
    step (Left err) _ = Left err
    step (Right acc) d
      | d < 0 || d >= base = Left (InvalidDigit d)
      | otherwise = Right (acc * base + d)

fromDecimal :: Integral a => a -> a -> [a]
fromDecimal _ 0 = []
fromDecimal base n = reverse (helper n)
  where
    helper 0 = []
    helper x = (x `mod` base) : helper (x `div` base)
  

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | otherwise = 
    case toDecimal inputBase inputDigits of
      Right n -> Right (fromDecimal outputBase n)
      Left err -> Left err
  
