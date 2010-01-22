module Generic.Data.Tuple where

import Prelude ()
import Generic.Data.Bool
import Generic.Data.Eq
import Generic.Data.Ord

class TupleC j where
  mkTuple :: j a -> j b -> j (a, b) 
  tuple   :: (j a -> j b -> j r) -> j (a, b) -> j r

instance (BoolC j, TupleC j, Eq j a, Eq j b) => Eq j (a, b) where
  x == y = tuple (\xa xb -> tuple (\ya yb -> xa == ya && xb == yb) y) x

instance (BoolC j, TupleC j, Eq j a, Eq j b, Ord j a, Ord j b) => Ord j (a, b) where
  x <= y = tuple (\xa xb -> tuple (\ya yb -> xa <= ya && xb <= yb) y) x

swap :: TupleC j => j (a, b) -> j (b, a)
swap = tuple (\a b -> mkTuple b a)

