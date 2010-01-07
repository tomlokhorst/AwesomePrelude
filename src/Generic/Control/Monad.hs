{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Generic.Control.Monad where

import Prelude ()
import Generic.Control.Applicative
import Generic.Control.Pointed
import Generic.Control.Category

class Applicative j m => Monad j m where
  (>>=) :: j (m a) -> (j a -> j (m b)) -> j (m b)

return :: Monad j m => j a -> j (m a)
return = pure

join :: (Category j (->), Monad j m) => j (m (m a)) -> j (m a)
join ma = ma >>= id

