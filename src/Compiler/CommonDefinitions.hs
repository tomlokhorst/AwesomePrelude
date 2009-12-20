module Compiler.CommonDefinitions where

import Compiler.Expr
import Compiler.Generics
import Compiler.LiftDefinitions
import Control.Arrow hiding (app)
import Data.Either
import Prelude hiding (lookup)

eliminate :: Arrow (~>) => Definitions ~> Definitions
eliminate = arr (Defs . fixpoint f . unDefs)
  where
  f []     = []
  f (d:ds) = d : eliminate1 d (f ds)

eliminate1 :: Definition -> [Definition] -> [Definition]
eliminate1 (Def n e) ds =
  let (subs, keeps) = partitionEithers (map part ds)
      part (Def m g) = if g == e && m /= n then Left m else Right (Def m g)
  in map (\(Def m g) -> Def m (foldr (substitute1 n) g subs)) keeps
  
substitute1 :: Var -> Var -> Expr -> Expr
substitute1 to from = rec
  where
    tr (App  f e ) = app (rec f) (rec e)
    tr (Con  c   ) = con c
    tr (Lam  ps e) = lam ps (rec e)
    tr (Name n e ) = name n (rec e)
    tr (Prim s vs) = prim s (map rep vs)
    tr (Var  u   ) = var (rep u)

    rep u = if u == from then to else u
    rec = tr . unId . out
