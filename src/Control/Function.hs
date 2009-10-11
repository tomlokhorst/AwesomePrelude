module Control.Function where

import qualified Prelude as P

-- infixr 9  .
infixr 0  $

class Fun f where
  lam :: (f a -> f b) -> f (a -> b)
  ($) :: f (a -> b) -> f a -> f b
  fix :: (f a -> f a) -> f a

lam2 :: Fun f => (f a -> f b -> f c) -> f (a -> b -> c)
lam2 f = lam (\a -> lam (\b -> f a b))

