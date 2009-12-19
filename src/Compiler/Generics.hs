module Compiler.Generics where

import Data.Foldable
import Data.Monoid

newtype FixA a f = In { out :: a f (FixA a f) }

newtype Id f a = Id { unId :: f a }

type Fix f = FixA Id f

foldA :: Functor f => (FixA a f -> f (FixA a f)) -> (f c -> c) -> FixA a f -> c
foldA un f = f . fmap (foldA un f) . un

foldId :: Functor f => (f c -> c) -> FixA Id f -> c
foldId = foldA (unId . out)

reduce :: (Monoid a, Foldable f) => (f (Fix f) -> a) -> Fix f -> a
reduce f = foldMap (\x -> f (unId (out x)) `mappend` reduce f x) . unId . out

