{-# LANGUAGE MultiParamTypeClasses #-}

module Generic.Control.Alternative where

import Prelude ()
import Generic.Control.Applicative

class Applicative j f => Alternative j f where
  empty :: j (f a)
  (<|>) :: j (f a) -> j (f a) -> j (f a)

