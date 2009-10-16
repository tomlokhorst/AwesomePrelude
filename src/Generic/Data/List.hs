module Generic.Data.List where

import Prelude ()
import Generic.Data.Number
import Generic.Data.Bool
import Generic.Control.Function
import Generic.Control.Functor

class ListC j where
  nil  :: j [a]
  cons :: j a -> j [a] -> j [a]
  list :: j r -> (j a -> j [a] -> j r) -> j [a] -> j r

instance (FunC j, ListC j) => Functor j [] where
  fmap f = foldr (\a r -> f a `cons` r) nil

singleton :: ListC j => j a -> j [a]
singleton a = cons a nil

(++) :: ListC j => j [a] -> j [a] -> j [a]
xs ++ ys = list ys cons xs

foldr :: (FunC j, ListC j) => (j a -> j b -> j b) -> j b -> j [a] -> j b
foldr f b xs = fix (\r -> lam (list b (\y ys -> f y (r `app` ys)))) `app` xs

length :: (FunC j, NumC j, ListC j) => j [a] -> j Num
length = foldr (\_ -> (+1)) 0

sum :: (FunC j, NumC j, ListC j) => j [Num] -> j Num
sum = foldr (+) 0

filter :: (ListC j, BoolC j, FunC j) => (j a -> j Bool) -> j [a] -> j [a]
filter p = foldr (\x xs -> bool xs (x `cons` xs) (p x)) nil

