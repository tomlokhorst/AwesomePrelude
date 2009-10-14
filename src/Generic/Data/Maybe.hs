module Generic.Data.Maybe where

import Prelude ()
import Generic.Data.List
import Generic.Control.Function

data Maybe a
class MaybeC j where
  nothing :: j (Maybe a)
  just    :: j a -> j (Maybe a)
  maybe   :: j r -> (j a -> j r) -> j (Maybe a) -> j r

fromMaybe :: MaybeC j => j a -> j (Maybe a) -> j a
fromMaybe d m = maybe d (\a -> a) m

catMaybes :: (FunC j, ListC j, MaybeC j) => j [Maybe a] -> j [a]
catMaybes = foldr (\a b -> maybe nil singleton a ++ b) nil

