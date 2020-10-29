module Finite where

class (Enum a, Bounded a) => Finite a where
    finite :: [a]
    finite = enumFromTo minBound maxBound

mFinite :: Finite a => [Maybe a]
mFinite = Nothing : map Just finite

-- instance Finite (Maybe Bool)


-- instance (Enum a, Bounded a) => Finite a where
--    finite = enumFromTo minBound maxBound
