{-# LANGUAGE
    NoImplicitPrelude
  , GADTs
  , MultiParamTypeClasses
  , TypeFamilies
  , FunctionalDependencies
  , FlexibleInstances
  , ScopedTypeVariables
  , FlexibleContexts
 #-}

module Generic.Data.List where

import Prelude (fromInteger)
import qualified Prelude as P
import Generic.Control.Function

class List f where
  nil  :: f [a]
  cons :: f a -> f [a] -> f [a]
  list :: f r -> (f a -> f [a] -> f r) -> f [a] -> f r

singleton :: List f => f a -> f [a]
singleton = P.flip cons nil

foldr :: (Fun f, List f) => (f a -> f b -> f b) -> f b -> f [a] -> f b
foldr f b xs = fix (\r -> lam (list b (\y ys -> f y (r $ ys)))) $ xs

sum :: (P.Num (f a), Fun f, List f) => f [a] -> f a
sum = foldr (P.+) 0

(++) :: List f => f [a] -> f [a] -> f [a]
xs ++ ys = list ys cons xs






--map :: (List f a (g b), List g b r) => (a -> b) -> f a -> g b
--map f = list nil (\x ys -> f x `cons` ys)

--filter :: (List f a (g a), Bool b (g a), List g a r) => (a -> b) -> f a -> g a
--filter p = list nil (\x xs -> bool xs (x `cons` xs) (p x))
