module Control.Function where

import qualified Prelude as P

infixr 9  .
infixr 0  $

class Fun f where
  id  :: f a a
  ($) :: f a b -> a -> b
  (.) :: f b c -> f a b -> f a c

instance Fun (->) where
  id  = P.id
  ($) = (P.$)
  (.) = (P..)

