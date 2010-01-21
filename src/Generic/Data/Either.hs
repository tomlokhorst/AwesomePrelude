{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Data.Either where

import Prelude ()
import Generic.Data.Bool
import Generic.Data.Eq
import Generic.Data.Ord
import Generic.Control.Function

data Either a b
class EitherC j where
  left   :: j a -> j (Either a b)
  right  :: j b -> j (Either a b)
  either :: (j a -> j r) -> (j b -> j r) -> j (Either a b) -> j r

instance (BoolC j, FunC j, EitherC j, Eq j a, Eq j b) => Eq j (Either a b) where
  ex == ey = either (\x -> either (\y -> x == y) (const false) ey)
                    (\x -> either (const false) (\y -> x == y) ey)
                    ex

instance (BoolC j, FunC j, EitherC j, Ord j a, Ord j b) => Ord j (Either a b) where
  ex <= ey = either (\x -> either (\y -> x <= y) (const true) ey)
                    (\x -> either (const false) (\y -> x <= y) ey)
                    ex

