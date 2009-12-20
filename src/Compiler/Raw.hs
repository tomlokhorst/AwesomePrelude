{-# LANGUAGE
    TypeFamilies
  , FlexibleContexts
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
 #-}
module Compiler.Raw where

import Compiler.Generics
import Control.Applicative
import Control.Monad.State
import Data.Foldable hiding (elem, mapM_, concatMap, concat, foldr)
import Data.Map (Map, lookup)
import Data.Maybe
import Data.Monoid
import Data.Traversable hiding (mapM)
import Prelude hiding (lookup)
import qualified Data.Reify as R
import qualified Data.Set as Set

-- Raw value datatype.
 
data ExprF f =
    App   f f
  | Con   String
  | Prim  ([String] -> String) [String]
  | Lam   [String] f
  | Var   String
  | Def   String f
  | More  [f]
  deriving (Functor, Foldable, Traversable)

type Expr = Fix ExprF

-- Smart constructors.

app :: Expr -> Expr -> Expr
app a b = In (Id (App a b))

con :: String -> Expr
con a = In (Id (Con a))

prim :: ([String] -> String) -> [String] -> Expr
prim f as = In (Id (Prim f as))

lam :: [String] -> Expr -> Expr
lam as f = In (Id (Lam as f))

var :: String -> Expr
var a = In (Id (Var a))

def :: String -> Expr -> Expr
def a b = In (Id (Def a b))

more :: [Expr] -> Expr
more as = In (Id (More as))

-- MuRef instances for Data.Reify.

instance Traversable f => R.MuRef (FixA Id f) where
  type R.DeRef (Fix f) = f
  mapDeRef f = traverse f . unId . out

data Graph = Graph
  { nodes :: Map String (ExprF String)
  , first :: String
  }

-- Generic value traversal.

foldGraph
  :: (Graph -> [a] -> [a] -> String -> String -> String                 -> a)
  -> (Graph               -> String -> String                           -> a)
  -> (Graph               -> String -> ([String] -> String) -> [String] -> a)
  -> (Graph -> [a]        -> String -> [String] -> String               -> a)
  -> (Graph               -> String -> String                           -> a)
  -> (Graph -> [a]        -> String -> String -> String                 -> a)
  -> (Graph -> [[a]]      -> String -> [String]                         -> a)
  -> Graph
  -> [a]
foldGraph f0 f1 f2 f3 f4 f5 f6 g@(Graph m r) = evalState (folder (r, r `from` m)) Set.empty
  where
    folder (i, term) =
      case term of
        App f b   -> rec f >>= \r0 -> rec b >>= \r1 -> pure [f0 g r0 r1 i f b  ]
        Con s     ->                                   pure [f1 g       i s    ]
        Prim s vs ->                                   pure [f2 g       i s vs ]
        Lam v b   -> rec b >>= \r0 ->                  pure [f3 g r0    i v b  ]
        Var v     ->                                   pure [f4 g       i v    ]
        More as   ->            mapM rec as >>= \rs -> pure [f6 g rs    i as   ]
        Def n b   -> rec b >>= \r0 ->                  pure [f5 g r0    i n b  ]
    from f = fromMaybe (error "internal error in foldGraph") . lookup f
    rec k =
      do v <- gets (Set.member k)
         modify (Set.insert k)
         if not v then folder (k, k `from` m) else return mempty

