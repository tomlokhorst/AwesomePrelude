module Compiler.FreeVariables
( FreeVarA (..)
, ExprFV
, annotateWithFreeVariables
, printDefinitionsWithFreeVariables
)
where

import Compiler.Generics
import Compiler.LiftDefinitions
import Compiler.Raw
import Control.Arrow hiding (app)
import Data.List (intercalate)
import Data.Set hiding (map, insert)
import qualified Data.Map as M

data FreeVarA f a = FreeVarA { free :: Set String , expr :: f a }

type ExprFV = FixA FreeVarA ExprF

annotateWithFreeVariables :: Arrow (~>) => Expr ~> ExprFV
annotateWithFreeVariables = arr ow
  where 
  ow ex = rec ex
    where

    -- references to global definitions don't count as free variables.
    globs = fromList (M.keys (collectDefinitions ex))

    -- traversal function
    ann (App l r)   = ae (union (fv l') (fv r'))           (App  l' r') where l' = rec l; r' = rec r
    ann (Con c)     = ae (empty)                           (Con  c    )                                             
    ann (Prim s vs) = ae (fromList vs)                     (Prim s vs )                                         
    ann (Lam x e)   = ae (fv e' `difference` fromList x)   (Lam  x e' ) where e' = rec e
    ann (Var v)     = ae (singleton v `difference` globs)  (Var  v    )                                             
    ann (Def n e)   = ae (fv e')                           (Def  n e' ) where e' = rec e
    ann (More es)   = ae (empty)                           (More es'  ) where es' = map rec es

    -- helpers
    rec = ann . unId . out
    ae vs e = In (FreeVarA vs e)
    fv = free . out

printDefinitionsWithFreeVariables :: Arrow (~>) => ExprFV ~> String
printDefinitionsWithFreeVariables = arr top
  where
  top (In (FreeVarA vf x)) = (if size vf /= 0 then "/* free: " ++ show (toList vf) ++ " */" else "/* 0 */") ++ tr x
  tr (App   f e)  = top f ++ "(" ++ top e ++ ")"
  tr (Con   c)    = c
  tr (Prim  s _)  = s
  tr (Lam   as e) = "(function (" ++ intercalate ", " as ++ ")" ++ "{ " ++ "return " ++ top e ++ ";" ++ " })"
  tr (Var   v)    = v
  tr (Def   n e)  = n ++ " = " ++ top e
  tr (More  es)   = intercalate "\n" (map top es)

