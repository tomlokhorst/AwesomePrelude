{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
 #-}

module HaskellPrelude where

import AwesomePrelude
import qualified Prelude as P

-- * Haskell instances for AwesomePrelude 'data types'

instance Bool P.Bool r where 
  bool f t x = if x then t else f
  true       = P.True
  false      = P.False

instance Maybe P.Maybe a r where
  maybe   = P.maybe
  nothing = P.Nothing
  just    = P.Just

instance Either P.Either a b r where
  either = P.either
  left   = P.Left
  right  = P.Right

instance Tuple2 (,) a b r where
  tuple2  f (x, y) = f x y
  ctuple2 x y      = (x, y)

-- instance Eq P.Ordering where
--   x == y = if x P.== y then true else false
-- 
-- instance Ordering P.Ordering r where
--   ordering x _ _ P.LT = x
--   ordering _ y _ P.EQ = y
--   ordering _ _ z P.GT = z
--   lt                  = P.LT
--   eq                  = P.EQ
--   gt                  = P.GT

instance List [] a r where
  list = \x f ys -> P.foldr f x ys
  nil  = []
  cons = (:)

-- * Haskell instances of AwesomePrelude 'type classes'

instance Eq P.Bool P.Bool where
  (==) = (P.==)
  (/=) = (P./=)

instance Eq P.Int P.Bool where
  (==) = (P.==)
  (/=) = (P./=)

