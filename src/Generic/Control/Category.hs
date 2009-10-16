module Generic.Control.Category where

import Prelude ()

infixr 9 .

class Category j (~>) where
  id  :: (j a ~> j a)
  (.) :: (j b ~> j c) -> (j a ~> j b) -> (j a ~> j c)

