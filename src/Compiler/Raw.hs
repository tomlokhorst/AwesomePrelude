{-# LANGUAGE
    TypeFamilies
  , FlexibleContexts
  , UndecidableInstances
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
 #-}
module Compiler.Raw where

import Control.Applicative
import Control.Monad.State
import Data.Foldable hiding (elem, mapM_, concatMap, concat, foldr)
import Data.Map
import Data.Maybe
import Data.Monoid
import Data.Traversable hiding (mapM)
import Prelude hiding (lookup)
import qualified Data.Reify as R
import qualified Data.Set as Set

-- Raw value datatype.
 
type Name = String

data ExprF f =
    App   f f
  | Prim  String
  | Lam   [Name] f
  | Var   Name
  | Name  Name f
  | More  [f]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Fix f = In { out :: f (Fix f) }

type Expr = Fix ExprF

-- Smart constructors.

app :: Expr -> Expr -> Expr
app a b = In (App a b )

prim :: String -> Expr
prim a = In (Prim a )

lam :: [Name] -> Expr -> Expr
lam as f = In (Lam as f)

var :: Name -> Expr
var a = In (Var a )

name :: Name -> Expr -> Expr
name a b = In (Name a b )

more :: [Expr] -> Expr
more as = In (More as )

instance Show (f (Fix f)) => Show (Fix f) where
  show (In f) = "(" ++ show f ++ ")"

instance Traversable a => R.MuRef (Fix a) where
  type R.DeRef (Fix a) = a
  mapDeRef f = traverse f . out

data Graph = Graph
  { nodes :: Map String (ExprF String)
  , first :: String
  }

-- Generic value traversal.

foldGraph
  :: (Graph -> [a] -> [a] -> String -> String -> String   -> a)
  -> (Graph               -> String -> String             -> a)
  -> (Graph -> [a]        -> String -> [String] -> String -> a)
  -> (Graph               -> String -> String             -> a)
  -> (Graph -> [a]        -> String -> String -> String   -> a)
  -> (Graph -> [[a]]      -> String -> [String]           -> a)
  -> Graph
  -> [a]
foldGraph f0 f1 f2 f3 f4 f5 g@(Graph m r) = evalState (folder (r, r `from` m)) Set.empty
  where
    folder (i, term) =
      case term of
        App f b  -> rec f >>= \r0 -> rec b >>= \r1 -> pure [f0 g r0 r1 i f b ]
        Prim s   ->                                   pure [f1 g       i s   ]
        Lam v b  -> rec b >>= \r0 ->                  pure [f2 g r0    i v b ]
        Var v    ->                                   pure [f3 g       i v   ]
        More as  ->            mapM rec as >>= \rs -> pure [f5 g rs    i as  ]
        Name n b -> rec b >>= \r0 ->                  pure [f4 g r0    i n b ]
    from f = fromMaybe (error "internal error in foldGraph") . lookup f
    rec k =
      do v <- gets (Set.member k)
         modify (Set.insert k)
         if not v then folder (k, k `from` m) else return mempty

