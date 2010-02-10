{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Data.Maybe where

import Prelude ()
import Generic.Data.Bool
import Generic.Data.Eq
import Generic.Data.List
import Generic.Data.Ord
import Generic.Control.Function
import Generic.Control.Category
import Generic.Control.Functor

data Maybe a
class MaybeC j where
  nothing :: j (Maybe a)
  just    :: j a -> j (Maybe a)
  maybe   :: j r -> (j a -> j r) -> j (Maybe a) -> j r

instance (BoolC j, FunC j, MaybeC j, Eq j a) => Eq j (Maybe a) where
  mx == my = maybe (maybe true (const false) my)
                   (\x -> maybe false (\y -> x == y) my)
                   mx

instance (BoolC j, FunC j, MaybeC j, Ord j a) => Ord j (Maybe a) where
  mx <= my = maybe true -- (maybe true (const true) my)
                   (\x -> maybe false (\y -> x <= y) my)
                   mx

instance (FunC j, MaybeC j) => Functor j Maybe where
  fmap f = maybe nothing (just . f)

fromMaybe :: MaybeC j => j a -> j (Maybe a) -> j a
fromMaybe d m = maybe d (\a -> a) m

catMaybes :: (RecFunC j, ListC j, MaybeC j) => j [Maybe a] -> j [a]
catMaybes = foldr (\a b -> maybe nil singleton a ++ b) nil

