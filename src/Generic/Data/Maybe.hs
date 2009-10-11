module Generic.Data.Maybe where

import Prelude ()
import Generic.Data.List
import Generic.Control.Function

data Maybe a
class MaybeC f where
  nothing :: f (Maybe a)
  just    :: f a -> f (Maybe a)
  maybe   :: f r -> (f a -> f r) -> f (Maybe a) -> f r

fromMaybe :: MaybeC f => f a -> f (Maybe a) -> f a
fromMaybe d m = maybe d (\a -> a) m

catMaybes :: (FunC f, ListC f, MaybeC f) => f [Maybe a] -> f [a]
catMaybes = foldr (\a b -> maybe nil singleton a ++ b) nil

