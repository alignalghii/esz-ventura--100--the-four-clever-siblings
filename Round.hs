module Round where

import SmallRational


data Round2 = Preexistent | Q SmallRational deriving (Eq, Ord, Show)

instance Bounded Round2 where
    minBound = Preexistent
    maxBound = Q maxBound

instance Enum Round2 where
    succ Preexistent = Q zeroQ
    succ (Q q)
        | q <= maxBound - halfQ = Q $ q + halfQ
        | otherwise             = error "maxBound smallrational has no successor"
    pred (Q q)
        | q >= halfQ = Q $ q - halfQ
        | otherwise  = Preexistent
    pred Preexistent = error "Preexistent has no predessor"
    toEnum n
        | n < 0     = Preexistent
        | otherwise = Q $ n `perQ` 2
    fromEnum Preexistent = -1
    fromEnum (Q q) = numeratorQ $ q * 2

minInf2 :: Round2
minInf2 = Preexistent

whole2, andHalf :: Int -> Round2
whole2 = Q . natQ
andHalf n = Q (natQ n + halfQ)

quarter4, threeQuarter4 :: Round2
quarter4      = Q quarterQ
threeQuarter4 = Q threeQuarterQ
andQuarter, andThreeQuarter :: Int -> Round2
andQuarter n      = Q (natQ n + quarterQ)
andThreeQuarter n = Q (natQ n + threeQuarterQ)
