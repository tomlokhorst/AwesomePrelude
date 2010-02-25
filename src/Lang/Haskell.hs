{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module Lang.Haskell where

import qualified Prelude as P

import Generic.Prelude

type family H a :: *

newtype Haskell a = Hs { runHaskell :: H a }


-- * Haskell instances for AwesomePrelude 'data types'.

instance NameC Haskell where
  named _ a = a -- drop name annotation, for now

type instance H (a -> b) = H a -> H b

instance LamFunC Haskell where
  lam f             = Hs (\x -> runHaskell (f (Hs x)))

instance AppFunC Haskell where
  app (Hs f) (Hs x) = Hs (f x)

instance RecFunC Haskell where
  fix f             = f (fix f)

type instance H Bool = P.Bool

instance BoolC Haskell where
  false           = Hs P.False
  true            = Hs P.True
  bool x y (Hs b) = if b then y else x

type instance H (Maybe a) = P.Maybe (H a)

instance MaybeC Haskell where
  nothing           = Hs P.Nothing
  just (Hs x)       = Hs (P.Just x)
  maybe n f (Hs mx) = P.maybe n (\x -> f (Hs x)) mx

type instance H (a, b) = (H a, H b)

instance TupleC Haskell where
  mkTuple (Hs x) (Hs y) = Hs (x, y)
  tuple f (Hs (x, y))   = f (Hs x) (Hs y)

type instance H (Either a b) = P.Either (H a) (H b)

instance EitherC Haskell where
  left  (Hs x)      = Hs (P.Left x)
  right (Hs y)      = Hs (P.Right y)
  either l r (Hs e) = P.either (\x -> l (Hs x)) (\y -> r (Hs y)) e

type instance H [a] = [H a]

instance ListC Haskell where
  nil                 = Hs []
  cons (Hs x) (Hs xs) = Hs (x:xs)
  list n c (Hs xs)    = case xs of { [] -> n; y:ys -> c (Hs y) (Hs ys) }


-- * Haskell instances of AwesomePrelude type classes.

instance (P.Num a) => Num Haskell a where
  (+) = (P.+)
  (-) = (P.-)
  (*) = (P.*)
  fromInteger = P.fromInteger

instance (P.Eq a) => Eq Haskell a where
  x == y = if x P.== y then true else false

