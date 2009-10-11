module Generic.Data.Either where

import qualified Prelude as P

class Either f where
  left   :: f a -> f (P.Either a b)
  right  :: f b -> f (P.Either a b)
  either :: (f a -> f r) -> (f b -> f r) -> f (P.Either a b) -> f r

