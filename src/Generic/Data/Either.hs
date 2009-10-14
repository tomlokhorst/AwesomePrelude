module Generic.Data.Either where

import Prelude ()

data Either a b
class EitherC j where
  left   :: j a -> j (Either a b)
  right  :: j b -> j (Either a b)
  either :: (j a -> j r) -> (j b -> j r) -> j (Either a b) -> j r

