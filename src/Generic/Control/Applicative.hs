module Generic.Control.Applicative where

import Prelude ()
import Generic.Control.Pointed

class Pointed j f => Applicative j f where
  (<*>) :: j (f (a -> b)) -> j (f a) -> j (f b)

