module Poker (bestHands) where

import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands = do
    parsed <- mapM parseHand hands
    let ranked = map (\(s, h) -> (s, handRank h)) (zip hands parsed)
        best = maximumBy (comparing snd) ranked
        winners = [s | (s, r) <- ranked, r == snd best]
    return winners

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Show)

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | Jack | Queen | King | Ace
    deriving (Eq, Ord, Enum, Bounded, Show)

type Card = (Rank, Suit)
type Hand = [Card]

parseHand :: String -> Maybe Hand
parseHand str = do
    cards <- mapM parseCard (words str)
    guard (length cards == 5)
    return cards

parseCard :: String -> Maybe Card
parseCard [r, s] = do
    rank <- parseRank [r]
    suit <- parseSuit s
    return (rank, suit)
parseCard [r1, r2, s] = do
    rank <- parseRank [r1, r2]
    suit <- parseSuit s
    return (rank, suit)
parseCard _ = Nothing

parseRank :: String -> Maybe Rank
parseRank "2"  = Just R2
parseRank "3"  = Just R3
parseRank "4"  = Just R4
parseRank "5"  = Just R5
parseRank "6"  = Just R6
parseRank "7"  = Just R7
parseRank "8"  = Just R8
parseRank "9"  = Just R9
parseRank "10" = Just R10
parseRank "J"  = Just Jack
parseRank "Q"  = Just Queen
parseRank "K"  = Just King
parseRank "A"  = Just Ace
parseRank _    = Nothing

parseSuit :: Char -> Maybe Suit
parseSuit 'C' = Just Clubs
parseSuit 'D' = Just Diamonds
parseSuit 'H' = Just Hearts
parseSuit 'S' = Just Spades
parseSuit _   = Nothing

handRank :: Hand -> (Int, [Rank])
handRank hand =
    let ranks = sortOn (Down . fromEnum) (map fst hand)
        grouped = sortOn (\g -> (Down (length g), Down (fromEnum (head g))))
                  (group (sort ranks))
        counts = map length grouped
        orderedRanks = concat grouped
        flush = all ((== snd (head hand)) . snd) hand
        straightRanks = isStraight ranks
    in case () of
        _ | isJust straightRanks && flush -> (8, fromJust straightRanks)
          | counts == [4,1]   -> (7, orderedRanks)
          | counts == [3,2]   -> (6, orderedRanks)
          | flush             -> (5, ranks)
          | isJust straightRanks -> (4, fromJust straightRanks)
          | counts == [3,1,1] -> (3, orderedRanks)
          | counts == [2,2,1] -> (2, orderedRanks)
          | counts == [2,1,1,1] -> (1, orderedRanks)
          | otherwise         -> (0, ranks)

isStraight :: [Rank] -> Maybe [Rank]
isStraight rs =
    let vals = sort (map fromEnum rs)
        wheel = sort [fromEnum Ace, fromEnum R2, fromEnum R3, fromEnum R4, fromEnum R5]
    in if vals == wheel
          then Just [R5, R4, R3, R2, Ace] 
       else if and (zipWith (\a b -> a + 1 == b) vals (tail vals))
          then Just (reverse (sort rs)) 
       else Nothing