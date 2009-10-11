module Data.Tuple where

class Tuple f where
  mkTuple :: f a -> f b -> f (a, b) 
  tuple   :: (f a -> f b -> f r) -> f (a, b) -> f r

swap :: Tuple f => f (a, b) -> f (b, a)
swap = tuple (flip mkTuple)

