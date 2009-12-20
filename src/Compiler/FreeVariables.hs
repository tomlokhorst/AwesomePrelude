module Compiler.FreeVariables
( FreeVarA (..)
, ExprFV
, annotateExpression
, annotateDefinitions
, dump
)
where

import Compiler.Generics
import Compiler.Raw
import Control.Arrow hiding (app)
import Data.List (intercalate)
import Data.Set hiding (map, insert)
import Compiler.LiftDefinitions (DefinitionsA (..), Definitions)

data FreeVarA f a = FreeVarA { free :: Set String , expr :: f a }

type ExprFV = FixA FreeVarA ExprF

annotateExpression :: Arrow (~>) => Set String -> Expr ~> ExprFV
annotateExpression globs = arr ow
  where 
  ow ex = rec ex
    where

    -- traversal function
    ann (App l r)   = ae (union (fv l') (fv r'))           (App  l' r') where l' = rec l; r' = rec r
    ann (Con c)     = ae (empty)                           (Con  c    )                                             
    ann (Prim s vs) = ae (fromList vs)                     (Prim s vs )                                         
    ann (Lam x e)   = ae (fv e' `difference` fromList x)   (Lam  x e' ) where e' = rec e
    ann (Var v)     = ae (singleton v `difference` globs)  (Var  v    )                                             
    ann (Name n e)  = ae (fv e')                           (Name n e' ) where e' = rec e

    -- helpers
    rec = ann . unId . out
    ae vs e = In (FreeVarA vs e)
    fv = free . out

type DefinitionsFV = DefinitionsA FreeVarA

annotateDefinitions :: Arrow (~>) => Definitions ~> DefinitionsFV
annotateDefinitions = arr $ \(Defs ds m) ->
  let globs = fromList (map fst ds)
      ann   = annotateExpression globs
  in Defs (map (fmap ann) ds) (ann m)

dump :: Arrow (~>) => DefinitionsFV ~> String
dump = arr def
  where
    def (Defs ds m) = intercalate "\n" (map single (ds ++ [("__main", m)]))
    single (d, e)   = d ++ " = " ++ rec e

    tr (App  f e ) = rec f ++ "(" ++ rec e ++ ")"
    tr (Con  c   ) = c
    tr (Prim s vf) = s vf
    tr (Lam  as e) = "function (" ++ intercalate ", " as ++ ") { return " ++ rec e ++ ";" ++ " }"
    tr (Var  v   ) = v
    tr (Name n e ) = "/* " ++ n ++ " */ " ++ rec e

    rec (In (FreeVarA vf x)) =
      if size vf /= 0
      then "/* free: " ++ show (toList vf) ++ " */" ++ tr x
      else "/* 0 */"                                ++ tr x

