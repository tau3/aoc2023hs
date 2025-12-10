{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Day7 (solvePt1, parse) where

import Data.List (sort)
import Data.List.NonEmpty (group)
import GHC.Exts (sortWith)

parse :: [String] -> [(Hand String, Int)]
parse xs = map (\s -> (makeHand (head s), read (s !! 1) :: Int)) splits
  where
    splits = map words xs

solvePt1 :: [(Hand String, Int)] -> Int
solvePt1 hands = sum $ map (\(index, (_, bid)) -> index * bid) iterated
  where
    sorted = sortWith fst hands
    iterated = zip [1 ..] sorted

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A deriving (Eq, Ord, Show)

makeCard :: Char -> Card
makeCard 'A' = A
makeCard 'K' = K
makeCard 'Q' = Q
makeCard 'J' = J
makeCard 'T' = T
makeCard '9' = Nine
makeCard '8' = Eight
makeCard '7' = Seven
makeCard '6' = Six
makeCard '5' = Five
makeCard '4' = Four
makeCard '3' = Three
makeCard '2' = Two
makeCard c = error $ "unknown card " ++ [c]

data Hand a where
  FiveOfAKind :: a -> Hand a
  FourOfAKind :: a -> Hand a
  FullHouse :: a -> Hand a
  ThreeOfAKind :: a -> Hand a
  TwoPair :: a -> Hand a
  OnePair :: a -> Hand a
  HighCard :: a -> Hand a
  deriving (Show)

instance (Eq a) => Eq (Hand a) where
  (==) (FiveOfAKind a) (FiveOfAKind b) = a == b
  (==) (FourOfAKind a) (FourOfAKind b) = a == b
  (==) (FullHouse a) (FullHouse b) = a == b
  (==) (ThreeOfAKind a) (ThreeOfAKind b) = a == b
  (==) (TwoPair a) (TwoPair b) = a == b
  (==) (OnePair a) (OnePair b) = a == b
  (==) (HighCard a) (HighCard b) = a == b
  (==) _ _ = False

compareSameHand :: String -> String -> Ordering
compareSameHand l r = cmp' l' r'
  where
    l' = map makeCard l
    r' = map makeCard r
    cmp' (x : xs) (y : ys) = if x == y then cmp' xs ys else compare x y
    cmp' _ _ = error $ "completely equal hands: " ++ l ++ " and " ++ r

instance Ord (Hand String) where
  compare (FiveOfAKind a) (FiveOfAKind b) = compareSameHand a b
  compare (FiveOfAKind _) _ = GT
  compare _ (FiveOfAKind _) = LT
  compare (FourOfAKind a) (FourOfAKind b) = compareSameHand a b
  compare (FourOfAKind _) _ = GT
  compare _ (FourOfAKind _) = LT
  compare (FullHouse a) (FullHouse b) = compareSameHand a b
  compare (FullHouse _) _ = GT
  compare _ (FullHouse _) = LT
  compare (ThreeOfAKind a) (ThreeOfAKind b) = compareSameHand a b
  compare (ThreeOfAKind _) _ = GT
  compare _ (ThreeOfAKind _) = LT
  compare (TwoPair a) (TwoPair b) = compareSameHand a b
  compare (TwoPair _) _ = GT
  compare _ (TwoPair _) = LT
  compare (OnePair a) (OnePair b) = compareSameHand a b
  compare (OnePair _) _ = GT
  compare _ (OnePair _) = GT
  compare (HighCard a) (HighCard b) = compareSameHand a b

makeHand :: String -> Hand String
makeHand hand
  | counts hand == [5] = FiveOfAKind hand
  | counts hand == [1, 4] = FourOfAKind hand
  | counts hand == [2, 3] = FullHouse hand
  | counts hand == [1, 1, 3] = ThreeOfAKind hand
  | counts hand == [1, 2, 2] = TwoPair hand
  | counts hand == [1, 1, 1, 2] = OnePair hand
  | otherwise = HighCard hand

counts :: String -> [Int]
counts hand = sort $ map length $ orderedGroups hand
  where
    orderedGroups = group . sort
