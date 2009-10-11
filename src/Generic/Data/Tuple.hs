module Generic.Data.Tuple where

import Prelude ()

class TupleC f where
  mkTuple :: f a -> f b -> f (a, b) 
  tuple   :: (f a -> f b -> f r) -> f (a, b) -> f r

swap :: TupleC f => f (a, b) -> f (b, a)
swap = tuple (\a b -> mkTuple b a)

