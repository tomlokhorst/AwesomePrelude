module Generic.Control.Category where

import Prelude ()

infixr 9 .

class Category j cat where
  id  :: cat (j a) (j a)
  (.) :: cat (j b) (j c)
      -> cat (j a) (j b)
      -> cat (j a) (j c)

