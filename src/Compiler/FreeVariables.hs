{-# LANGUAGE TypeOperators #-}

module Compiler.FreeVariables
( FreeVarA (..)
, ExpressionFV
, annotateExpression
, DefinitionFV
, DefinitionsFV
, annotateDefinitions
, dump
)
where

import Compiler.Generics
import Compiler.Expression
import Control.Arrow hiding (app)
import Data.List (intercalate)
import Data.Set hiding (map, insert)
import Compiler.LiftDefinitions (DefinitionsA (..), Definitions, DefinitionA (..))

data FreeVarA f a = FreeVarA { free :: Set String , expr :: f a }

type ExpressionFV = FixA FreeVarA ExpressionF

annotateExpression :: Arrow (~>) => Set String -> Expression ~> ExpressionFV
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

type DefinitionFV  = DefinitionA  FreeVarA
type DefinitionsFV = DefinitionsA FreeVarA

annotateDefinitions :: Arrow (~>) => Definitions ~> DefinitionsFV
annotateDefinitions = arr $ \(Defs ds) ->
  let globs = fromList (map defName ds)
      ann   = annotateExpression globs
  in Defs (map (\(Def n e) -> Def n (ann e)) ds)

dump :: Arrow (~>) => DefinitionsFV ~> String
dump = arr (intercalate "\n" . map one . unDefs)
  where
    one (Def d e) = "var " ++ d ++ " = " ++ rec e

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

