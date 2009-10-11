module Data.AMaybe where

import Data.AwesomeList
import Control.Function
import qualified Prelude as P

class Maybe f where
  nothing :: f (P.Maybe a)
  just    :: f a -> f (P.Maybe a)
  maybe   :: f r -> (f a -> f r) -> f (P.Maybe a) -> f r

fromMaybe :: Maybe f => f a -> f (P.Maybe a) -> f a
fromMaybe d m = maybe d (\a -> a) m

catMaybes :: (List f, Maybe f, Fun f) => f [P.Maybe a] -> f [a]
catMaybes = foldr (\a b -> maybe nil singleton a ++ b) nil

