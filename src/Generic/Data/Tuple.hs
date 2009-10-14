module Generic.Data.Tuple where

import Prelude ()

class TupleC j where
  mkTuple :: j a -> j b -> j (a, b) 
  tuple   :: (j a -> j b -> j r) -> j (a, b) -> j r

swap :: TupleC j => j (a, b) -> j (b, a)
swap = tuple (\a b -> mkTuple b a)

