module Princess where

import Type4 (Type4)
import Partition4
import Finite

-- Should use dependent types
-- Reimplement it in Coq, Agda or Idris
-- Or refactory it in Haskell with type promotion

data Princess = Ps (Maybe Type4) (Partition4) deriving Show

allPrincesses :: Int -> [Princess]
allPrincesses n = do
    p <- allPartitions4 n
    mt <- mFinite
    return $ Ps mt p

netSum, grossSum, mixSum :: Princess -> Int
netSum  (Ps _ p) = sum4 p
grossSum = succ . netSum
mixSum  (Ps (Just _) p) = sum4 p + 1
mixSum  (Ps Nothing  p) = sum4 p
