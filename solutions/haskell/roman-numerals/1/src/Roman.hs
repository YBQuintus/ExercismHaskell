module Roman (numerals) where

numeralRecursion :: Integer -> String
numeralRecursion n
  | n >= 1000 = 'M':(numeralRecursion (n-1000))
  | n >= 900 = "CM" ++ (numeralRecursion (n-900))
  | n >= 500 = 'D':(numeralRecursion (n-500))
  | n >= 400 = "CD" ++ (numeralRecursion (n-400))
  | n >= 100 = 'C':(numeralRecursion (n-100))
  | n >= 90 = "XC" ++ (numeralRecursion (n-90))
  | n >= 50 = 'L':(numeralRecursion (n-50))
  | n >= 40 = "XL" ++ (numeralRecursion (n-40))
  | n >= 10 = 'X':(numeralRecursion (n-10))
  | n >= 9 = "IX" ++ (numeralRecursion (n-9))
  | n >= 5 = 'V':(numeralRecursion (n-5))
  | n >= 4 = "IV" ++ (numeralRecursion (n-4))
  | n >= 1 = 'I':(numeralRecursion (n-1))
  | otherwise = ""
  

numerals :: Integer -> Maybe String
numerals n 
  | n <= 0 || n >= 4000 = Nothing
  | otherwise = Just (numeralRecursion n)
