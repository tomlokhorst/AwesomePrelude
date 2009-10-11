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
import Control.Function

class ListC f l a where
  nil  :: f (l a)
  cons :: f a -> f (l a) -> f (l a)

class ListC f l a => ListD f l a r where
  listD :: f r -> (f a -> f (l a) -> f r) -> f (l a) -> f r

foldr
  :: (Fun f, ListD f l a b)
  => (f a -> f b -> f b) -> f b -> f (l a) -> f b
foldr f b xs = fix (\r -> lam (listD b (\y ys -> f y (r $ ys)))) $ xs

sum :: (P.Num (f a), Fun f, ListD f l a a) => f (l a) -> f a
sum = foldr (P.+) 0







--map :: (List f a (g b), List g b r) => (a -> b) -> f a -> g b
--map f = list nil (\x ys -> f x `cons` ys)
--
--(++) :: (List f a (g a), List g a r) => f a -> g a -> g a
--xs ++ ys = list ys cons xs
--
--filter :: (List f a (g a), Bool b (g a), List g a r) =>
--            (a -> b) -> f a -> g a
--filter p = list nil (\x xs -> bool xs (x `cons` xs) (p x))

--foldrB f b = fix (\r -> foldrA f b r)


-- mapA :: (ListC g b, ListD f a (g b))
--          => (a -> b) -> f a -> g b
-- mapA f = foldrA (\x ys -> f x `consC` ys) nilC
-- 
-- sumA :: (P.Num a, ListD l a a) => l a -> a
-- sumA = foldrA (P.+) 0

