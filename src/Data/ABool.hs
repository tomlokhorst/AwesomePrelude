module Data.ABool where

import qualified Prelude as P

class Bool f where
  true  :: f P.Bool
  false :: f P.Bool
  bool  :: f a -> f a -> f P.Bool -> f a

not :: Bool f => f P.Bool -> f P.Bool
not = bool false true

and :: Bool f => f P.Bool -> f P.Bool -> f P.Bool
and a b = bool b false a

or :: Bool f => f P.Bool -> f P.Bool -> f P.Bool
or a b = bool true b a

class Eq f a where
  (==) :: f a -> f a -> f P.Bool
  (/=) :: f a -> f a -> f P.Bool

