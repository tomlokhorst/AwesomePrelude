{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
 #-}

module Lang.Haskell where

import AwesomePrelude
import qualified Prelude as P
import Generic.Data.List

-- * Haskell instances for AwesomePrelude 'data types'

instance Fix a where
  fix f = f (fix f)

instance BoolC P.Bool where 
  trueC       = P.True
  falseC      = P.False

instance BoolD P.Bool r where 
  boolD f t x = if x then t else f

instance Bool P.Bool r where 
  bool f t x = if x then t else f
  true       = P.True
  false      = P.False

instance MaybeC P.Maybe a where
  nothingC = P.Nothing
  justC    = P.Just

instance MaybeD P.Maybe a r where
  maybeD   = P.maybe

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

instance ListC [] a where
  nil  = []
  cons = (:)

instance ListD [] a r where
  listD x f []     = x
  listD x f (y:xs) = f y xs

-- * Haskell instances of AwesomePrelude 'type classes'

instance Eq P.Bool P.Bool where
  (==) = (P.==)
  (/=) = (P./=)

instance Eq P.Int P.Bool where
  (==) = (P.==)
  (/=) = (P./=)

