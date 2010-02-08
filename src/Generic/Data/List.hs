{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Data.List where

import Prelude ()
import Generic.Data.Bool
import Generic.Data.Eq
import Generic.Data.Num
import Generic.Data.Ord
import Generic.Control.Function
import Generic.Control.Functor

-- data List a
-- instead of `List a`, use `[a]`

class ListC j where
  nil  :: j [a]
  cons :: j a -> j [a] -> j [a]
  list :: j r -> (j a -> j [a] -> j r) -> j [a] -> j r

instance (BoolC j, ListC j, Eq j a) => Eq j [a] where
  xs == ys = list (list true (\_ _ -> false) ys)
                  (\x xs' -> list false (\y ys' -> x == y && xs' == ys') ys)
                  xs

instance (BoolC j, ListC j, Ord j a) => Ord j [a] where
  xs <= ys = list true -- (list true (\_ _ -> true) ys)
                  (\x xs' -> list false (\y ys' -> x <= y || xs' <= ys') ys)
                  xs

instance (FunC j, ListC j) => Functor j [] where
  fmap f = foldr (\a r -> f a `cons` r) nil

singleton :: ListC j => j a -> j [a]
singleton a = a `cons` nil

foldr :: (FunC j, ListC j) => (j a -> j b -> j b) -> j b -> j [a] -> j b
foldr f b xs = fix (\r -> lam (list b (\y ys -> f y (r `app` ys)))) `app` xs

replicate :: (ListC j, Eq j a, BoolC j, FunC j, Num j a) => j a -> j b -> j [b]
replicate n a = fix (\r -> lam (\y -> bool nil (a `cons` (r `app` (y - 1))) (y == 0))) `app` n

(++) :: (FunC j, ListC j) => j [a] -> j [a] -> j [a]
xs ++ ys = foldr cons ys xs

genericLength :: (FunC j, ListC j, Num j a) => j [b] -> j a
genericLength = foldr (\_ -> (+1)) 0

sum :: (FunC j, ListC j, Num j a) => j [a] -> j a
sum = foldr (+) 0

filter :: (ListC j, BoolC j, FunC j) => (j a -> j Bool) -> j [a] -> j [a]
filter p = foldr (\x xs -> bool xs (x `cons` xs) (p x)) nil

reverse :: (FunC j, ListC j) => j [a] -> j [a]
reverse l = rev `app` l `app` nil
  where
    rev = fix (\r -> lam (\xs -> lam (\a -> list a (\y ys -> r `app` ys `app` (y `cons` a)) xs)))

