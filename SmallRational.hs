module SmallRational where

import Data.Ratio


data SmallRational = SR (Ratio Int) deriving (Eq, Ord, Show)

instance Bounded SmallRational where
    minBound = zeroQ
    maxBound = maxQ

instance Num SmallRational where
    SR a + SR b   = SR $ a + b
    SR a - SR b   = SR $ a - b
    SR a * SR b   = SR $ a * b
    abs (SR a)    = SR $ abs a
    signum (SR a) = SR $ signum a
    fromInteger   = SR . fromInteger

zeroQ, halfQ, maxQ :: SmallRational
zeroQ = SR 0
halfQ = SR $ 1 % 2
maxQ  = natQ maxBound

quarterQ, threeQuarterQ :: SmallRational
quarterQ      = SR $ 1 % 4
threeQuarterQ = SR $ 3 % 4

numeratorQ, denominatorQ :: SmallRational -> Int
numeratorQ   (SR a) = numerator   a
denominatorQ (SR a) = denominator a

perQ :: Int -> Int -> SmallRational
perQ m n = SR $ m % n

natQ :: Int -> SmallRational
natQ = SR . (% 1)
