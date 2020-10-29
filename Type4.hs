module Type4 where

import Finite

data Type4 = N0Per4 | N1Per4 | N2Per4 | N3Per4 deriving (Show, Enum, Bounded)

type4_ind :: a -> a -> a -> a -> Type4 -> a
type4_ind c0 _  _  _  N0Per4 = c0
type4_ind _  c1 _  _  N1Per4 = c1
type4_ind _  _  c2 _  N2Per4 = c2
type4_ind _  _  _  c3 N3Per4 = c3

instance Finite Type4

opposite4 :: Type4 -> Type4
opposite4 N0Per4 = N2Per4
opposite4 N1Per4 = N3Per4
opposite4 N2Per4 = N0Per4
opposite4 N3Per4 = N1Per4
