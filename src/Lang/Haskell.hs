{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Lang.Haskell where

import Prelude ((++))
import qualified Prelude as P

import Generic.Prelude

-- Short, concise name for Identiy wrapper
newtype Id a = Id { unId :: a }
  deriving P.Show

runHaskell :: Id a -> a
runHaskell = unId

-- * Haskell instances for AwesomePrelude 'data types'.

instance NameC Id where
  named s a = a -- drop name annotation, for now

instance FunC Id where
  lam f             = Id (\x -> unId (f (Id x)))
  fix f             = f (fix f)
  app (Id f) (Id x) = Id (f x)

