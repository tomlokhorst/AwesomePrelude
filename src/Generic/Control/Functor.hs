{-# LANGUAGE MultiParamTypeClasses #-}

module Generic.Control.Functor where

import Prelude ()

class Functor j f where
  fmap :: (j a -> j b) -> j (f a) -> j (f b)

