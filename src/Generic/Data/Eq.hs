{-# LANGUAGE MultiParamTypeClasses #-}

module Generic.Data.Eq where

import Prelude ()
import Generic.Data.Bool

infix  4  ==, /=

class Eq j a where
  (==) :: j a -> j a -> j Bool
  (/=) :: j a -> j a -> j Bool

