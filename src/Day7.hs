{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Day7 (solvePt1) where

import Data.List (sort)
import Data.List.NonEmpty (group)
import GHC.Exts (sortWith)

solvePt1 :: [(Hand String, Int)] -> Int
solvePt1 hands = sum $ map (\(index, (_, bid)) -> index * bid) iterated
  where
    sorted = sortWith fst hands
    losers = init sorted
    iterated = zip [1 ..] losers

data Card = A | K | Q | J | T | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving (Eq, Ord)

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
makeCard _ = error "unknown card"

data Hand a where
  FiveOfAKind :: a -> Hand a
  FourOfAKind :: a -> Hand a
  FullHouse :: a -> Hand a
  ThreeOfAKind :: a -> Hand a
  TwoPair :: a -> Hand a
  OnePair :: a -> Hand a
  HighCard :: a -> Hand a

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
makeHand xs
  | counts xs == [5] = FiveOfAKind xs
  | counts xs == [1, 4] = FourOfAKind xs
  | counts xs == [2, 3] = FullHouse xs
  | counts xs == [1, 1, 3] = ThreeOfAKind xs
  | counts xs == [1, 2, 2] = TwoPair xs
  | counts xs == [1, 1, 1, 2] = OnePair xs
  | otherwise = HighCard xs

counts :: String -> [Int]
counts xs = sort $ map length $ asd xs
  where
    asd = group . sort
