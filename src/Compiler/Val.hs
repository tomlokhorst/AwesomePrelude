{-# LANGUAGE
    GADTs
  , KindSignatures
  , EmptyDataDecls
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , FunctionalDependencies
  , TypeOperators
 #-}
module Core.Val where

import Control.Monad.Identity
import Control.Monad.State

-- Core types.

data List a
data Number
data Boolean
data Text

data Type = Fun | Con | In | Out | InOut | Cast
  deriving (Eq, Ord, Show)

-- Indexed FRP values.

data Val :: * -> * where
  App   :: Val (a -> b) -> Val a -> Val b
  Prim  :: Type -> String -> String -> Val a

infixr 1 :->
infixr 2 :~>
type a :-> b = Val a -> b
type a :~> b = Val a -> Val b

prim :: Type -> String -> String -> a :~> b
prim t q f a = Prim t q f `App` a

prim2 :: Type -> String -> String -> a :-> b :~> c
prim2 t q f a b = Prim t q f `App` a `App` b

prim3 :: Type -> String -> String -> a :-> b :-> c :~> d
prim3 t q f a b c = Prim t q f `App` a `App` b `App` c

instance Eq (Val a) where
  (==) = undefined

instance Show (Val a) where
  show = undefined

-- Category instance for easy composing.

-- The FRP monad is a state monad that saves dependency graphs.

type FRP a = StateT [Val ()] Identity a

infixl 1 <~

(<~) :: a :-> a :-> FRP ()
(<~) a b = modify (Arr a b:)

(<~>) :: a :-> a :-> FRP ()
(<~>) a b = (a <~ b) >> (b <~ a)

-- Primitive conversions.

class ToText a where
  text :: Val a -> Val Text

instance ToText Text where
  text = prim Cast "cast" "/*cast*/"

-- Lift constant values into nodes.

class Show a => Const a b | a -> b where
  con :: a -> Val b

instance Const [Char] Text where
  con s = Prim Con (show s) (show s)

instance Const Int Number where
  con i = Prim Con (show i) (show i)

instance Const Float Number where
  con i = Prim Con (show i) (show i)

instance Const Bool Boolean where
  con False = Prim Con "T" "false"
  con True  = Prim Con "F" "true"

