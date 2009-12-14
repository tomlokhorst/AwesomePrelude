{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Compiler.Raw where

import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Foldable hiding (elem, mapM_, concatMap, concat, foldr)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid
import Data.List (group, sort)
import Data.Reify
import Data.Traversable hiding (mapM)
import qualified Lang.Value as Ix
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

-- Raw value datatype.
 
data Val f =
    App f f
  | Prim String
  | Lam [String] f
  | Var String
  | Name String f
  | More [f] -- HACK!!!!!!
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

-- Useful instances.

instance Functor Val where
  fmap f (App g a)  = App (f g) (f a)
  fmap _ (Prim s)   = Prim s
  fmap f (Lam v b)  = Lam v (f b)
  fmap _ (Var v)    = Var v
  fmap f (Name n a) = Name n (f a)
  fmap f (More as)  = More (fmap f as)

instance Foldable Val where
  fold (App f a)  = f `mappend` a
  fold (Prim _)   = mempty
  fold (Lam _ b)  = b
  fold (Var _)    = mempty
  fold (Name _ a) = a
  fold (More as)  = mconcat as

instance Traversable Val where
  traverse f (App g a)  = App <$> f g <*> f a
  traverse _ (Prim s)   = pure (Prim s)
  traverse f (Lam v b)  = Lam v <$> f b
  traverse _ (Var v)    = pure (Var v)
  traverse f (Name n a) = Name n <$> f a
  traverse f (More as)  = More <$> traverse f as

instance Traversable a => MuRef (Fix a) where
  type DeRef (Fix a) = a
  mapDeRef f = traverse f . out

type ValMap = Map.Map String (Val String)

blaat :: Show (Ix.Primitive l) => Ix.Val l i -> IO ()
blaat n = (Map.toList . fst . renamer <$> reifyGraph (raw n)) >>= mapM_ print

renamer :: Graph Val -> (ValMap, String)
renamer (Graph xs r) =
  let strings = map (\(u, tu) -> (show u, fmap show tu)) xs
      named   = catMaybes (map getName strings)
      getName (from, Name to f) = Just (from, to, f)
      getName _                 = Nothing
  in (foldr replacer (Map.fromList strings) named, show r)
  where
  replacer (from, to, f) m =
    let Just x = Map.lookup f m
        rep from to x = if x == from then to else x
    in Map.delete from
     $ Map.insert to x
     $ Map.map (fmap (rep from to))
     $ m

-- inliner (vm, x) = 
--   let uses = map snd . filter ((<=1) . fst) . map (length &&& head) . group . sort . concatMap toList . Map.elems $ vm :: [String]
--       rep from to x = if x == from then to else x
--       once (f, t) = Map.map (fmap (rep f t))
--   in undefined -- (foldr once vm uses, x)

-- Generic value traversal.

foldVal
  :: (ValMap -> [a] -> [a] -> String -> String -> String -> a)
  -> (ValMap               -> String -> String           -> a)
  -> (ValMap -> [a]        -> String -> [String] -> String -> a)
  -> (ValMap               -> String -> String           -> a)
  -> (ValMap -> [a]        -> String -> String -> String -> a)
  -> (ValMap -> [[a]]      -> String -> [String]         -> a)
  -> (ValMap, String)
  -> [a]
foldVal f0 f1 f2 f3 f4 f5 (m, r) = evalState (folder (r, r `from` m)) Set.empty
  where
    folder (i, term) =
      case term of
        App f b  -> rec f >>= \r0 -> rec b >>= \r1 -> pure [f0 m r0 r1 i f b ]
        Prim s   ->                                   pure [f1 m       i s   ]
        Lam v b  -> rec b >>= \r0 ->                  pure [f2 m r0    i v b ]
        Var v    ->                                   pure [f3 m       i v   ]
        More as  ->            mapM rec as >>= \rs -> pure [f5 m rs    i as  ]
        Name n b -> rec b >>= \r0 ->                  pure [f4 m r0    i n b ]
        where rec k =
                do v <- gets (Set.member k)
                   modify (Set.insert k)
                   mboolM (folder (k, k `from` m)) (not v)
    from f = fromMaybe (error "internal error in foldVal") . Map.lookup f

mboolM :: (Monad m, Monoid a) => m a -> Bool -> m a
mboolM a b = if b then a else return mempty

