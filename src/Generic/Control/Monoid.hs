module Generic.Control.Monoid where

import Prelude ()

infixr 6 <>

class Monoid j a where
  zero :: j a
  (<>) :: j a -> j a -> j a

