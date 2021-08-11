{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Data.Bag
  ( Bag
  , emptyBag
  , singleton
  , fromList
  , toList
  , add
  , remove
  , member
  , count
  , unique
  ) where

import Prelude hiding (map)

import qualified Data.Map.Strict as M
import Data.Mappable (Mappable(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup)
import qualified Data.Set as S

-- | Datatype for Bag
data Bag a =
  Bag
    { getMap :: M.Map a Int
    , len :: Int
    }
  deriving (Eq, Ord, Show)

-- | Empty Bag of type `a`. O(1)
emptyBag :: Bag a
emptyBag = Bag M.empty 0

-- | Bag containing one element of type `a`. O(1)
singleton :: a -> Bag a
singleton a = Bag (M.singleton a 1) 1

-- | Function to add one element to a Bag of the same type. O(lg n)
add :: Ord a => a -> Bag a -> Bag a
add a (Bag m l) = Bag (M.insertWith (+) a 1 m) (l + 1)

-- | Function to check if an element is present in the Bag or not. Use `member` in place of `elem`. O(lg n)
member :: Ord a => a -> Bag a -> Bool
member a = M.member a . getMap

-- | Function to count the occurence of an element in the Bag. O(lg n)
count :: (Eq a, Ord a) => a -> Bag a -> Int
count a = fromMaybe 0 . M.lookup a . getMap

-- | Function to remove one occurence of an element from the Bag. O(lg n)
remove :: Ord a => a -> Bag a -> Bag a
remove a b@(Bag m l)
  | count a b == 0 = b
  | count a b == 1 = Bag (M.delete a m) (l - 1)
  | otherwise = Bag (M.insertWith (-) a 1 m) (l - 1)

-- | Function to return unique elements from a Bag. O(# of distinct elements)
unique :: Ord a => Bag a -> S.Set a
unique = S.fromList . M.keys . getMap

-- | Function to convert a Bag into a List. O(n)
toList :: Bag a -> [a]
toList = M.foldlWithKey appendToListNTimes [] . getMap
  where
    appendToListNTimes :: [a] -> a -> Int -> [a]
    appendToListNTimes as a n
      | n <= 0 = as
      | otherwise = appendToListNTimes (a : as) a (n - 1)

-- | Function to create a Bag from a List. O(m lg m + n), where m = # of distinct elements in the List, and n = # of elements in the List
fromList :: Ord a => [a] -> Bag a
fromList = foldMap singleton

-- | Foldable instance for Bag
instance Foldable Bag
  --foldr :: (a -> b -> b) -> b -> Bag a -> b
                                               where
  foldr f b = foldr f b . toList
  length = len
  minimum = fst . M.findMin . getMap
  maximum = fst . M.findMax . getMap

-- | Semigroup instance for Bag
instance Ord a => Semigroup (Bag a) where
  (Bag m1 l1) <> (Bag m2 l2) = Bag (M.unionWith (+) m1 m2) (l1 + l2)

-- | Monoid instance for Bag
instance Ord a => Monoid (Bag a) where
  mempty = emptyBag

-- | Mappable instance for Bag
instance (Ord a, Ord b) => Mappable Bag a b
    -- map :: (a -> b) -> Bag a -> Bag b
                                          where
  map f = fromList . fmap f . toList
