{-# LANGUAGE TypeFamilies #-}

module ToyLib.FromVec where

import qualified Data.Heap as H
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.MultiSet
import qualified Data.Set as S
import qualified Data.Vector.Generic as VG

class FromVec a where
  type FromVecInput a
  type FromVecItem a
  fromVec :: (VG.Vector v (FromVecInput a)) => v (FromVecInput a) -> a
  fromVecWith :: (VG.Vector v (FromVecInput a)) => (FromVecItem a -> FromVecItem a -> FromVecItem a) -> v (FromVecInput a) -> a
  fromVecWith _ = fromVec

instance FromVec (IM.IntMap a) where
  type FromVecInput (IM.IntMap a) = (Int, a)
  type FromVecItem (IM.IntMap a) = a
  fromVec = VG.foldl' (\im (!k, !v) -> IM.insert k v im) IM.empty
  fromVecWith !f = VG.foldl' (\im (!k, !v) -> IM.insertWith f k v im) IM.empty

instance (Ord k) => FromVec (M.Map k a) where
  type FromVecInput (M.Map k a) = (k, a)
  type FromVecItem (M.Map k a) = a
  fromVec = VG.foldl' (\im (!k, !v) -> M.insert k v im) M.empty
  fromVecWith !f = VG.foldl' (\im (!k, !v) -> M.insertWith f k v im) M.empty

instance FromVec IS.IntSet where
  type FromVecInput IS.IntSet = Int
  type FromVecItem IS.IntSet = Int
  fromVec = VG.foldl' (flip IS.insert) IS.empty
  fromVecWith _ = fromVec

instance (Ord a) => FromVec (S.Set a) where
  type FromVecInput (S.Set a) = a
  type FromVecItem (S.Set a) = a
  fromVec = VG.foldl' (flip S.insert) S.empty
  fromVecWith _ = fromVec

instance (Ord a) => FromVec (H.Heap a) where
  type FromVecInput (H.Heap a) = a
  type FromVecItem (H.Heap a) = a
  fromVec = VG.foldl' (flip H.insert) H.empty
  fromVecWith _ = fromVec

instance FromVec MultiSet where
  type FromVecInput MultiSet = Int
  type FromVecItem MultiSet = Int
  fromVec = VG.foldl' (flip incMS) emptyMS
  fromVecWith _ = fromVec

fromVecIM :: (VG.Vector v (Int, a)) => v (Int, a) -> IM.IntMap a
fromVecIM = fromVec

fromVecM :: (VG.Vector v (Int, a)) => v (Int, a) -> IM.IntMap a
fromVecM = fromVec

fromVecIS :: (VG.Vector v Int) => v Int -> IS.IntSet
fromVecIS = fromVec

fromVecS :: (Ord a, VG.Vector v a) => v a -> S.Set a
fromVecS = fromVec

fromVecH :: (Ord a, VG.Vector v a) => v a -> H.Heap a
fromVecH = fromVec
