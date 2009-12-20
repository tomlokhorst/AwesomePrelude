{-# LANGUAGE
    TypeFamilies
  , FlexibleContexts
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
 #-}
module Compiler.Raw where

import Compiler.Generics
import Control.Monad.State
import Data.Foldable hiding (elem, mapM_, concatMap, concat, foldr)
import Data.Traversable hiding (mapM)
import Prelude hiding (lookup)

-- Raw value datatype.

type Con  = String
type Name = String
type Var  = String
type Body = [Var] -> String

data ExprF f =
    App   f f
  | Con   Con
  | Lam   [Var] f
  | Name  String f
  | Prim  Body [Var]
  | Var   Var
  deriving (Functor, Foldable, Traversable)

type ExprA a = FixA a ExprF
type Expr    = Fix ExprF

-- Smart constructors.

app :: Expr -> Expr -> Expr
app a b = In (Id (App a b))

con :: Con -> Expr
con a = In (Id (Con a))

lam :: [Var] -> Expr -> Expr
lam as f = In (Id (Lam as f))

name :: Name -> Expr -> Expr
name a b = In (Id (Name a b))

prim :: Body -> [Var] -> Expr
prim f as = In (Id (Prim f as))

var :: Var -> Expr
var a = In (Id (Var a))

