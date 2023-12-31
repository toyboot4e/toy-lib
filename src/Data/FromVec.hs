{-# LANGUAGE TypeFamilies #-}

-- | Helper methods for creating collections types from @Vector@ types.
module Data.FromVec where

import qualified Data.Heap as H
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.MultiSet
import qualified Data.Set as S
import qualified Data.Vector.Generic as G

class FromVec a where
  type FromVecInput a
  type FromVecItem a
  fromVec :: (G.Vector v (FromVecInput a)) => v (FromVecInput a) -> a
  fromVecWith :: (G.Vector v (FromVecInput a)) => (FromVecItem a -> FromVecItem a -> FromVecItem a) -> v (FromVecInput a) -> a
  fromVecWith _ = fromVec

instance FromVec (IM.IntMap a) where
  type FromVecInput (IM.IntMap a) = (Int, a)
  type FromVecItem (IM.IntMap a) = a
  fromVec = G.foldl' (\im (!k, !v) -> IM.insert k v im) IM.empty
  fromVecWith !f = G.foldl' (\im (!k, !v) -> IM.insertWith f k v im) IM.empty

instance (Ord k) => FromVec (M.Map k a) where
  type FromVecInput (M.Map k a) = (k, a)
  type FromVecItem (M.Map k a) = a
  fromVec = G.foldl' (\im (!k, !v) -> M.insert k v im) M.empty
  fromVecWith !f = G.foldl' (\im (!k, !v) -> M.insertWith f k v im) M.empty

instance FromVec IS.IntSet where
  type FromVecInput IS.IntSet = Int
  type FromVecItem IS.IntSet = Int
  fromVec = G.foldl' (flip IS.insert) IS.empty
  fromVecWith _ = fromVec

instance (Ord a) => FromVec (S.Set a) where
  type FromVecInput (S.Set a) = a
  type FromVecItem (S.Set a) = a
  fromVec = G.foldl' (flip S.insert) S.empty
  fromVecWith _ = fromVec

instance (Ord a) => FromVec (H.Heap a) where
  type FromVecInput (H.Heap a) = a
  type FromVecItem (H.Heap a) = a
  fromVec = G.foldl' (flip H.insert) H.empty
  fromVecWith _ = fromVec

instance FromVec MultiSet where
  type FromVecInput MultiSet = Int
  type FromVecItem MultiSet = Int
  fromVec = G.foldl' (flip incMS) emptyMS
  fromVecWith _ = fromVec

-- | Strongly typed `fromVec`.
fromVecIM :: (G.Vector v (Int, a)) => v (Int, a) -> IM.IntMap a
fromVecIM = fromVec

-- | Strongly typed `fromVecWith`.
fromVecWithIM :: (G.Vector v (Int, a)) => (a -> a -> a) -> v (Int, a) -> IM.IntMap a
fromVecWithIM = fromVecWith

-- | Strongly typed `fromVec`.
fromVecM :: (G.Vector v (k, a), Ord k) => v (k, a) -> M.Map k a
fromVecM = fromVec

-- | Strongly typed `fromVecWith`.
fromVecWithM :: (G.Vector v (k, a), Ord k) => (a -> a -> a) -> v (k, a) -> M.Map k a
fromVecWithM = fromVecWith

-- | Strongly typed `fromVec`.
fromVecIS :: (G.Vector v Int) => v Int -> IS.IntSet
fromVecIS = fromVec

-- | Strongly typed `fromVec`.
fromVecS :: (Ord a, G.Vector v a) => v a -> S.Set a
fromVecS = fromVec

-- | Strongly typed `fromVec`.
fromVecH :: (Ord a, G.Vector v a) => v a -> H.Heap a
fromVecH = fromVec
