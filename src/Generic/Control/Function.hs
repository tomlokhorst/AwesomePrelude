{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Control.Function where

import qualified Prelude
import Generic.Control.Category

undefined :: a
undefined = Prelude.undefined

class NameC j where
  named :: Prelude.String -> j a -> j a

class NameC j => FunC j where
  lam :: (j a -> j b) -> j (a -> b)
  app :: j (a -> b) -> j a -> j b

class FunC j => RecFunC j where
  fix :: (j (a -> b) -> j (a -> b)) -> j (a -> b)

instance FunC j => Category j (->) where
  id a      = "id" `named` (lam (\i -> i) `app` a)
  (.) f g a = lam f `app` (lam g `app` a)

infixr 0 $

($) :: FunC j => (j a -> j b) -> j a -> j b
($) f a = lam f `app` a

const :: FunC j => j a -> j b -> j a
const a b = lam2 (\c _ -> c) `app` a `app` b

-- Helper functions.

lam2 :: FunC j => (j a -> j b -> j c) -> j (a -> b -> c)
lam2 f = lam (\a -> lam (f a))

lam3 :: FunC j => (j a -> j b -> j c -> j d) -> j (a -> b -> c -> d)
lam3 f = lam (\a -> lam2 (f a))

app2 :: FunC j => j (a -> b -> c) -> j a -> j b -> j c
app2 f x y = (f `app` x) `app` y

app3 :: FunC j => j (a -> b -> c -> d) -> j a -> j b -> j c -> j d
app3 f x y z = app2 f x y `app` z

