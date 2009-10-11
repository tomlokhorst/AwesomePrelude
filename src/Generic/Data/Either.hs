module Generic.Data.Either where

import Prelude ()

data Either a b
class EitherC f where
  left   :: f a -> f (Either a b)
  right  :: f b -> f (Either a b)
  either :: (f a -> f r) -> (f b -> f r) -> f (Either a b) -> f r

