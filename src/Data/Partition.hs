module Data.Partition
  ( Partition
  , discrete
  , tribe
  , tribes
  , expel
  , unite
  , united
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

newtype Partition a = Partition { unP :: Map a (Set a) }

instance Ord a => Eq (Partition a) where
  px == py = tribes px == tribes py

discrete :: Partition a
discrete = Partition Map.empty

tribe :: Ord a => a -> Partition a -> Set a
tribe x = fromMaybe (Set.singleton x) . Map.lookup x . unP

tribes :: Ord a => Partition a -> [Set a]
tribes (Partition p) = case Map.minView p of
  Nothing -> []
  Just (xs, p') -> xs : tribes (Partition (p' `Map.withoutKeys` xs))

expel :: Ord a => a -> Partition a -> Partition a
expel x p
  | x `Map.notMember` unP p = p
  | otherwise = Partition $ Map.delete x $ Map.union (Map.fromSet (const newSet) newSet) $ unP p
  where newSet = Set.delete x (tribe x p)

unite :: Ord a => a -> a -> Partition a -> Partition a
unite x y p
  | united x y p = p
  | otherwise = Partition $ Map.union (Map.fromSet (const newSet) newSet) $ unP p
  where newSet = Set.union (tribe x p) (tribe y p)

united :: Ord a => a -> a -> Partition a -> Bool
united x y p = x `Set.member` tribe y p
