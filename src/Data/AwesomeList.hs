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

module Data.AwesomeList where

import Prelude (fromInteger)
import qualified Prelude as P

class Fix a where
  fix :: (a -> a) -> a

class ListC l a where
  nil  :: l a
  cons :: a -> l a -> l a

class (ListC l a) => ListD l a r where
  listD :: r -> (a -> l a -> r) -> l a -> r

foldr
  :: (ListD l a r, Fix (l a -> r)) => (a -> r -> r) -> r -> l a -> r
foldr f b = fix (\r -> listD b (\y ys -> f y (r ys)))

--map :: (List f a (g b), List g b r) => (a -> b) -> f a -> g b
--map f = list nil (\x ys -> f x `cons` ys)
--
--(++) :: (List f a (g a), List g a r) => f a -> g a -> g a
--xs ++ ys = list ys cons xs
--
--filter :: (List f a (g a), Bool b (g a), List g a r) =>
--            (a -> b) -> f a -> g a
--filter p = list nil (\x xs -> bool xs (x `cons` xs) (p x))

sum :: (P.Num a, ListD l a a, Fix (l a -> a)) => l a -> a
sum = foldr (P.+) 0

--foldrB f b = fix (\r -> foldrA f b r)


-- mapA :: (ListC g b, ListD f a (g b))
--          => (a -> b) -> f a -> g b
-- mapA f = foldrA (\x ys -> f x `consC` ys) nilC
-- 
-- sumA :: (P.Num a, ListD l a a) => l a -> a
-- sumA = foldrA (P.+) 0

