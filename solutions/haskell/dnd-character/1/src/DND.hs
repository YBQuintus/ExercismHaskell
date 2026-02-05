module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Data.List
import Test.QuickCheck (Gen, choose)
import Control.Monad

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier constitution = (constitution - 10) `div` 2

ability :: Gen Int
ability = do
  rolls <- replicateM 4 (choose (1,6))
  let topThree = tail (sort rolls)
  return (sum topThree)

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return $ Character str dex con int wis cha hp
