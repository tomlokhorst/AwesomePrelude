module Generic.Control.Function where

import qualified Prelude

undefined :: a
undefined = Prelude.undefined

class FunC f where
  lam :: (f a -> f b) -> f (a -> b)
  app :: f (a -> b) -> f a -> f b
  fix :: (f a -> f a) -> f a

-- Could as well use default definitions of ($) and (.).

infixr 9 .
infixr 0 $

($) :: (a -> b) -> a -> b
($) f a = f a

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g a = f (g a)

lam2 :: FunC f => (f a -> f b -> f c) -> f (a -> b -> c)
lam2 f = lam (\a -> lam (f a))

lam3 :: FunC f => (f a -> f b -> f c -> f d) -> f (a -> b -> c -> d)
lam3 f = lam (\a -> lam2 (f a))

app2 :: FunC f => f (a -> b -> c) -> f a -> f b -> f c
app2 f x y = (f `app` x) `app` y

app3 :: FunC f => f (a -> b -> c -> d) -> f a -> f b -> f c -> f d
app3 f x y z = app2 f x y `app` z

