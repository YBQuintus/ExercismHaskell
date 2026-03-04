module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

checkTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
checkTriangle a b c 
  | any (== 0) [a,b,c] = False
  | otherwise = (a + b >= c) && (b + c >= a) && (a + c >= b) 

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c = 
  if checkTriangle a b c then
    if a == b && a == c then
      Equilateral
    else
      if a == b || a == c || b == c then
        Isosceles
      else
        Scalene
  else
    Illegal
