module Generic.Control.Monad where

import Prelude ()
import Generic.Control.Applicative
import Generic.Control.Pointed

class Applicative j m => Monad j m where
  (>>=) :: j (m a) -> (j a -> j (m b)) -> j (m b)

return :: Monad j m => j a -> j (m a)
return = pure

