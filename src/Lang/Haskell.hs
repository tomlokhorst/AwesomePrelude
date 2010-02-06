{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module Lang.Haskell where

import Prelude ((++))
import qualified Prelude as P

import Generic.Prelude

type family H a :: *

newtype Haskell a = Hs { runHaskell :: H a }

-- * Haskell instances for AwesomePrelude 'data types'.

instance NameC Haskell where
  named _ a = a -- drop name annotation, for now

type instance H (a -> b) = H a -> H b

instance FunC Haskell where
  lam f             = Hs (\x -> runHaskell (f (Hs x)))
  fix f             = f (fix f)
  app (Hs f) (Hs x) = Hs (f x)

type instance H Bool = P.Bool

instance BoolC Haskell where
  false = Hs P.False
  true  = Hs P.True
  bool x y (Hs b) = if b then y else x

