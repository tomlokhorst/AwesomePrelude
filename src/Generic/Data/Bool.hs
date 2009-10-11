module Generic.Data.Bool where

import Prelude ()

data Bool
class BoolC f where
  true  :: f Bool
  false :: f Bool
  bool  :: f a -> f a -> f Bool -> f a

not :: BoolC f => f Bool -> f Bool
not = bool false true

and :: BoolC f => f Bool -> f Bool -> f Bool
and a b = bool b false a

or :: BoolC f => f Bool -> f Bool -> f Bool
or a b = bool true b a

class Eq f a where
  (==) :: f a -> f a -> f Bool
  (/=) :: f a -> f a -> f Bool

