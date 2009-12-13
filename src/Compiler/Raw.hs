{-# LANGUAGE UndecidableInstances #-}
module Compiler.Raw where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
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
  | Lam [String] f
  | Var String
  | Name String f
  deriving (Eq, Ord, Show)

newtype Fix f = In { out :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  show (In f) = "(" ++ show f ++ ")"

-- Conversion from type indexed values to raw values.

raw :: Show (Ix.Primitive l) => Ix.Val l i -> Fix Val
raw = flip runReader 0 . tr
  where
  tr :: (Show (Ix.Primitive l), Applicative m, MonadReader Integer m) => Ix.Val l i -> m (Fix Val)
  tr (Ix.App f a)  = (\g b -> In (App g b)) <$> tr f <*> tr a
  tr (Ix.Prim s)   = pure (In (Prim (show s)))
  tr (Ix.Lam f)    = local (+1) (ask >>= \r -> In . Lam ['v':show r] <$> tr (f (Ix.Var r)))
  tr (Ix.Var x)    = pure (In (Var ('v':show x)))
  tr (Ix.Name x v) = (\a -> In (Name x a)) <$> tr v

-- Dealing with multiple values.

fromValues :: Show (Ix.Primitive l) => Ix.Val l i -> IO (Graph Val)
fromValues = fmap CSE.cse . reifyGraph . raw

-- Useful instances.

instance Functor Val where
  fmap f (App g a)  = App (f g) (f a)
  fmap _ (Prim s)   = Prim s
  fmap f (Lam v b)  = Lam v (f b)
  fmap _ (Var v)    = Var v
  fmap f (Name n a) = Name n (f a)

instance Foldable Val where
  fold (App f a)  = f `mappend` a
  fold (Prim _)   = mempty
  fold (Lam _ b)  = b
  fold (Var _)    = mempty
  fold (Name _ a) = a

instance Traversable Val where
  traverse f (App g a)  = App <$> f g <*> f a
  traverse _ (Prim s)   = pure (Prim s)
  traverse f (Lam v b)  = Lam v <$> f b
  traverse _ (Var v)    = pure (Var v)
  traverse f (Name n a) = Name n <$> f a

instance Traversable a => MuRef (Fix a) where
  type DeRef (Fix a) = a
  mapDeRef f = traverse f . out

-- Generic value traversal.

type NodeMap = Map.Map Int (Val Int)

from :: Ord k => k -> Map.Map k a -> a
from f = fromMaybe (error "internal error in foldVal") . Map.lookup f

foldVal
  :: (NodeMap -> [a] -> [a] -> Int -> Int -> Int    -> a)
  -> (NodeMap               -> Int -> String        -> a)
  -> (NodeMap -> [a]        -> Int -> [String] -> Int -> a)
  -> (NodeMap               -> Int -> String        -> a)
  -> (NodeMap -> [a]        -> Int -> String -> Int -> a)
  -> Graph Val
  -> [a]
foldVal f0 f1 f2 f3 f4 (Graph xs r) = evalState (folder (r, r `from` m)) Set.empty
  where
    m = Map.fromList xs
    folder (i, term) =
      case term of
        App f b  -> rec f >>= \r0 -> rec b >>= \r1 -> pure [f0 m r0 r1 i f b ]
        Prim s   ->                                   pure [f1 m       i s   ]
        Lam v b  ->                  rec b >>= \r0 -> pure [f2 m r0    i v b ]
        Var v    ->                                   pure [f3 m       i v   ]
        Name n b ->                  rec b >>= \r0 -> pure [f4 m r0    i n b ]
        where rec k =
                do v <- gets (Set.member k)
                   modify (Set.insert k)
                   mboolM (folder (k, k `from` m)) (not v)

mboolM :: (Monad m, Monoid a) => m a -> Bool -> m a
mboolM a b = if b then a else return mempty

