{-# LANGUAGE MultiParamTypeClasses #-}

module Generic.Data.Eq where

import Prelude ()
import Generic.Data.Bool

infix  4  ==, /=

class Eq j a where
  (==) :: (BoolC j) => j a -> j a -> j Bool
  (/=) :: (BoolC j) => j a -> j a -> j Bool

  x /= y = not (x == y)
  x == y = not (x /= y)

instance (BoolC j) => Eq j Bool where
  x == y = if' x y (not y)

z :: (BoolC j) => j Bool
z = true == false

