{-# LANGUAGE
    TypeFamilies
  , FlexibleContexts
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
 #-}
module Compiler.Expression where

import Compiler.Generics
import Control.Monad.State
import Data.Foldable hiding (elem, mapM_, concatMap, concat, foldr)
import Data.Traversable hiding (mapM)
import Prelude hiding (lookup)

-- Expression datatype.

type Con  = String
type Name = String
type Var  = String
type Body = [Var] -> String

data ExpressionF f =
    App   f f
  | Con   Con
  | Lam   [Var] f
  | Name  String f
  | Prim  Body [Var]
  | Var   Var
  deriving (Functor, Foldable, Traversable)

instance Eq f => Eq (ExpressionF f) where
  App   f g  == App   h i  = f == h && g == i
  Con   c    == Con   d    = c == d
  Lam   vs e == Lam   ws f = vs == ws && e == f
  Name  n e  == Name  m f  = n == m && e == f
  Prim  b vs == Prim  c ws = b vs == c ws
  Var   v    == Var   w    = v == w
  _          == _          = False

type ExpressionA a = FixA a ExpressionF
type Expression    = Fix ExpressionF

-- Smart constructors.

app :: Expression -> Expression -> Expression
app a b = In (Id (App a b))

con :: Con -> Expression
con a = In (Id (Con a))

lam :: [Var] -> Expression -> Expression
lam as f = In (Id (Lam as f))

name :: Name -> Expression -> Expression
name a b = In (Id (Name a b))

prim :: Body -> [Var] -> Expression
prim f as = In (Id (Prim f as))

var :: Var -> Expression
var a = In (Id (Var a))

