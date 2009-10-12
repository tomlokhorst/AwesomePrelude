{-# LANGUAGE UndecidableInstances #-}
module Compiler.Raw where

import Control.Applicative
import Control.Monad.State
import Control.Monad
import Data.Foldable hiding (elem, mapM_, concatMap, concat)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Reify
import Data.Traversable hiding (mapM)
import qualified Lang.Value as Ix
import qualified Data.Reify.Graph.CSE as CSE
import qualified Data.Map as Map
import qualified Data.IntSet as Set

-- Raw value datatype.

data Val f =
    App f f
  | Prim String
  deriving (Eq, Ord, Show)

newtype Fix f = In { out :: f (Fix f) }

-- instance Show (f (Fix f)) => Show (Fix f) where
--   show (In f) = "(" ++ show f ++ ")"

-- Conversion from type indexed values to raw values.

raw :: Show (Ix.Primitive l) => Ix.Val l i -> Fix Val
raw v = evalState (tr v) 0
  where
  tr :: (Show (Ix.Primitive l), Applicative m, MonadState Integer m)
     => Ix.Val l i -> m (Fix Val)
  tr (Ix.App f a) = (\g b -> In (App g b)) <$> tr f <*> tr a
  tr (Ix.Prim s)  = pure (In (Prim (show s)))
  tr (Ix.Lam f)   = modify (+1) >> get >>= \r -> tr (f (Ix.Var r))
  tr (Ix.Var x)   = pure (In (Prim (show x)))

-- Dealing with multiple values.

fromValues :: Show (Ix.Primitive l) => Ix.Val l i -> IO (Graph Val)
fromValues = fmap CSE.cse . reifyGraph . raw

-- Useful instances.

instance Functor Val where
  fmap f (App g a) = App (f g) (f a)
  fmap _ (Prim s)  = Prim s

instance Foldable Val where
  fold (App f a) = f `mappend` a
  fold (Prim _)  = mempty

instance Traversable Val where
  traverse f (App g a) = App <$> f g <*> f a
  traverse _ (Prim s)  = pure (Prim s)

instance Traversable a => MuRef (Fix a) where
  type DeRef (Fix a) = a
  mapDeRef f = traverse f . out

-- Generic value traversal.

type Nodes = Map.Map Int (Val Int)

from :: Ord k => k -> Map.Map k a -> a
from f = fromMaybe (error "internal error in foldVal") . Map.lookup f

foldVal
  :: (Nodes -> Int -> Int -> Int -> a)
  -> (Nodes -> Int -> String -> a)
  -> Graph Val
  -> [a]
foldVal f0 f1 (Graph xs r) = evalState (folder (r, r `from` m)) Set.empty
  where
    m = Map.fromList xs
    folder (i, term) =
      let rec k = gets (Set.member k) <* modify (Set.insert k) >>=
                  mboolM (folder (k, k `from` m)) . not
      in case term of
        App f a -> (\x y z -> x ++ y ++ z) <$> rec f <*> rec a <*> pure [f0 m i f a]
        Prim s  ->                                                 pure [f1 m i s]

mboolM :: (Monad m, Monoid a) => m a -> Bool -> m a
mboolM a b = if b then a else return mempty

