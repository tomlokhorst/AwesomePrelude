module Generic.Data.List where

import Prelude ()
import Generic.Data.Number
import Generic.Control.Function

class ListC f where
  nil  :: f [a]
  cons :: f a -> f [a] -> f [a]
  list :: f r -> (f a -> f [a] -> f r) -> f [a] -> f r

singleton :: ListC f => f a -> f [a]
singleton a = cons a nil

foldr :: (FunC f, ListC f) => (f a -> f b -> f b) -> f b -> f [a] -> f b
foldr f b xs = fix (\r -> lam (list b (\y ys -> f y (r `app` ys)))) `app` xs

sum :: (FunC f, NumC f, ListC f) => f [Num] -> f Num
sum = foldr (+) 0

(++) :: ListC f => f [a] -> f [a] -> f [a]
xs ++ ys = list ys cons xs






--map :: (ListC f a (g b), ListC g b r) => (a -> b) -> f a -> g b
--map f = list nil (\x ys -> f x `cons` ys)

--filter :: (ListC f a (g a), Bool b (g a), ListC g a r) => (a -> b) -> f a -> g a
--filter p = list nil (\x xs -> bool xs (x `cons` xs) (p x))
