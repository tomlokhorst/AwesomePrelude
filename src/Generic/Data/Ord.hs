{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Data.Ord where

import Prelude ()
import Generic.Data.Bool
import Generic.Data.Eq

infix  4  <, <=, >=, >

class (Eq j a) => Ord j a where
  compare              :: (BoolC j, OrderingC j) => j a -> j a -> j Ordering
  (<), (<=), (>), (>=) :: (BoolC j, OrderingC j) => j a -> j a -> j Bool
  max, min             :: (BoolC j, OrderingC j) => j a -> j a -> j a

  compare x y = if' (x == y)
                    eq
                    (if' (x <= y) lt gt)

  x <  y = ordering true  false false (compare x y)
  x <= y = ordering true  true  false (compare x y)
  x >  y = ordering false false true  (compare x y)
  x >= y = ordering false true  true  (compare x y)

  max x y = if' (x <= y) y x
  min x y = if' (x <= y) x y

data Ordering
class OrderingC j where
  lt       :: j Ordering
  eq       :: j Ordering
  gt       :: j Ordering
  ordering :: j a -> j a -> j a -> j Ordering -> j a

instance (BoolC j) => Ord j Bool where
  x <= y = bool true -- (bool true true y)
                (bool false true y)
                x

instance (BoolC j, OrderingC j) => Eq j Ordering where
  x == y = ordering (ordering true  false false y)
                    (ordering false true  false y)
                    (ordering false false true  y)
                    x

instance (BoolC j, OrderingC j) => Ord j Ordering where
  x <= y = ordering true -- (ordering true  true  true y)
                    (ordering false true  true y)
                    (ordering false false true y)
                    x

comparing :: (Ord j a, BoolC j, OrderingC j)
          => (b -> j a) -> b -> b -> j Ordering
comparing p x y = compare (p x) (p y)

