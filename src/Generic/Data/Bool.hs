module Generic.Data.Bool where

import Prelude ()

data Bool
class BoolC j where
  true  :: j Bool
  false :: j Bool
  bool  :: j a -> j a -> j Bool -> j a

not :: BoolC j => j Bool -> j Bool
not = bool false true

and :: BoolC j => j Bool -> j Bool -> j Bool
and a b = bool b false a

or :: BoolC j => j Bool -> j Bool -> j Bool
or a b = bool true b a

class Eq j a where
  (==) :: j a -> j a -> j Bool
  (/=) :: j a -> j a -> j Bool

