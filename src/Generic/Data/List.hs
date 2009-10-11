module Generic.Data.List where

import Prelude ()
import Generic.Data.Number
import Generic.Data.Bool
import Generic.Control.Function

class ListC f where
  nil  :: f [a]
  cons :: f a -> f [a] -> f [a]
  list :: f r -> (f a -> f [a] -> f r) -> f [a] -> f r

singleton :: ListC f => f a -> f [a]
singleton a = cons a nil

(++) :: ListC f => f [a] -> f [a] -> f [a]
xs ++ ys = list ys cons xs

foldr :: (FunC f, ListC f) => (f a -> f b -> f b) -> f b -> f [a] -> f b
foldr f b xs = fix (\r -> lam (list b (\y ys -> f y (r `app` ys)))) `app` xs

sum :: (FunC f, NumC f, ListC f) => f [Num] -> f Num
sum = foldr (+) 0

map :: (ListC f, FunC f) => (f a -> f b) -> f [a] -> f [b]
map f = foldr (\a r -> f a `cons` r) nil

filter :: (ListC f, BoolC f, FunC f) => (f a -> f Bool) -> f [a] -> f [a]
filter p = foldr (\x xs -> bool xs (x `cons` xs) (p x)) nil

