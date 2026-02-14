module Bowling (score, BowlingError(..)) where

data BowlingError
  = IncompleteGame
  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  validateRolls rolls
  scoreFrames 1 rolls

validateRolls :: [Int] -> Either BowlingError ()
validateRolls =
  sequence_ . zipWith check [0..]
  where
    check i r
      | r < 0 || r > 10 = Left (InvalidRoll i r)
      | otherwise       = Right ()

scoreFrames :: Int -> [Int] -> Either BowlingError Int
scoreFrames frame rs
  | frame > 10 = Left (InvalidRoll 20 0)
  | frame == 10 = scoreTenth rs
  | otherwise =
      case rs of

        (10:b1:b2:rest) -> do
          next <- scoreFrames (frame+1) (b1:b2:rest)
          pure $ 10 + b1 + b2 + next

        (r1:r2:b:rest)
          | r1 + r2 == 10 -> do
              next <- scoreFrames (frame+1) (b:rest)
              pure $ 10 + b + next

        (r1:r2:rest)
          | r1 + r2 < 10 -> do
              next <- scoreFrames (frame+1) rest
              pure $ r1 + r2 + next

        (r1:r2:_)
          | r1 + r2 > 10 ->
              Left (InvalidRoll 1 r2)

        _ -> Left IncompleteGame

scoreTenth :: [Int] -> Either BowlingError Int

scoreTenth (10:b1:b2:[])
  | b1 /= 10 && b1 + b2 > 10 =
      Left (InvalidRoll 20 b2)  
  | otherwise =
      Right (10 + b1 + b2)

scoreTenth (10:b1:b2:x:_) = Left (InvalidRoll 21 x)

scoreTenth (r1:r2:b:[]) 
  | r1 + r2 == 10 =
      Right (10 + b)

scoreTenth (r1:r2:b:x:_) = Left (InvalidRoll 21 x)

scoreTenth (r1:r2:[])
  | r1 + r2 < 10 =
      Right (r1 + r2)
  | r1 + r2 > 10 = 
      Left IncompleteGame

scoreTenth (r1:r2:_)
  | r1 + r2 < 10 = 
    Left (InvalidRoll 20 r1 )

scoreTenth _ =
  Left IncompleteGame
