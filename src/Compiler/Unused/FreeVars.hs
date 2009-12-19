module Compiler.FreeVars (FreeVarA (..), ExprFV, annotateWithFreeVars) where

import Compiler.Raw
import Control.Arrow hiding (app)
import Data.Set hiding (map)

data FreeVarA f a = FreeVarA { free :: Set String , expr :: f a }

type ExprFV = FixA FreeVarA ExprF

annotateWithFreeVars :: Arrow (~>) => Expr ~> FixA FreeVarA ExprF
annotateWithFreeVars = arr (ann . unId . out)
  where

  -- traversal
  ann (App l r)   = ae (union (fv l') (fv r'))           (App  l' r') where l' = rec l; r' = rec r
  ann (Con c)     = ae (empty)                           (Con  c    )                                             
  ann (Prim s vs) = ae (fromList vs)                     (Prim s vs )                                         
  ann (Lam x e)   = ae (difference (fv e') (fromList x)) (Lam  x e' ) where e' = rec e
  ann (Var v)     = ae (singleton v)                     (Var  v    )                                             
  ann (Def n e)   = ae (fv e')                           (Def  n e' ) where e' = rec e
  ann (More es)   = ae (unions (map fv es'))             (More es'  ) where es' = map rec es

  -- helpers
  rec = annotateWithFreeVars
  ae vs e = In (FreeVarA vs e)
  fv = free . out

