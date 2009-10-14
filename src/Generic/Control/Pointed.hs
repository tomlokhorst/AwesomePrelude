module Generic.Control.Pointed where

import Prelude ()
import Generic.Control.Functor

class Functor j f => Pointed j f where
  pure :: j a -> j (f a)

