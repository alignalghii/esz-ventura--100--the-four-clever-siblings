{-# LANGUAGE NPlusKPatterns #-}

module Partition4 where

-- Should use dependent types
-- Reimplement it in Coq, Agda or Idris
-- Or refactory it in Haskell with type promotion

import Type4
import TupleExt
import Data.List (nub, sort)
import Finite (finite)

data Partition4 = P4 Int Int Int Int deriving (Eq, Ord)

instance Show Partition4 where
    showsPrec _ = shows . toTuple


partition4_ind :: (Int -> Int -> Int -> Int -> y) -> Partition4 -> y
partition4_ind f (P4 n0 n1 n2 n3) = f n0 n1 n2 n3

toTuple :: Partition4 -> (Int, Int, Int, Int)
toTuple = partition4_ind mkTuple4

toList :: Partition4 -> [Int]
toList = partition4_ind $ \n0 n1 n2 n3 -> [n0, n1, n2, n3]

isBalanced :: Partition4 -> Bool
isBalanced = (== 1) . length . nub . toList

sum4 :: Partition4 -> Int
sum4 = partition4_ind $ \n0 n1 n2 n3 -> n0 + n1 + n2 + n3

mapSoPer4 :: Type4 -> (Int -> Int) -> Partition4 -> Partition4
mapSoPer4 (N0Per4) f (P4 n0 n1 n2 n3) = P4 (f n0) n1 n2 n3
mapSoPer4 (N1Per4) f (P4 n0 n1 n2 n3) = P4 n0 (f n1) n2 n3
mapSoPer4 (N2Per4) f (P4 n0 n1 n2 n3) = P4 n0 n1 (f n2) n3
mapSoPer4 (N3Per4) f (P4 n0 n1 n2 n3) = P4 n0 n1 n2 (f n3)
-- mapSoPer4 which f = partition4_ind ... type4_ind ...

allPartitions4 :: Int -> [Partition4]
allPartitions4 n = do
    a0 <- [0 .. n]
    a1 <- [0 .. n - a0]
    a2 <- [0 .. n - a0 - a1]
    let a3 = n - a0 - a1 - a2
    return $ P4 a0 a1 a2 a3

allPartitions4' :: Int -> [Partition4]
allPartitions4' 0       = [P4 0 0 0 0]
allPartitions4' (n + 1) = sort $ nub $ allPartitions4' n >>= flip map finite . flip (flip mapSoPer4 succ)

theorems, theorem1, theorem2, theorem3 :: Bool
theorems = theorem1 && theorem2 && theorem3
theorem1 = all (\n -> allPartitions4 n == allPartitions4' n) [0 .. 10]
theorem2 = all (== 100) $ map sum4 $ allPartitions4 100
theorem3 = (filter isBalanced $ allPartitions4 100) == [P4 25 25 25 25]
