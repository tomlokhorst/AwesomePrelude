module Compiler.LiftDefinitions 
( DefTable
, DefinitionsA (..)
, Definitions
, inline
, collect
, lift
, dump
)
where

import Compiler.Generics
import Compiler.Raw 
import Control.Arrow hiding (app)
import Data.List (intercalate)

type DefTable a = [(String, FixA a ExprF)]

data DefinitionsA a = Defs
  { definitions :: DefTable a
  , expression  :: FixA a ExprF
  }

type Definitions = DefinitionsA Id

-- All named sub-expressions will be replaces by a variables that references
-- the definition that will be created. All named sub-expression MUST NOT
-- contain any free variables.

inline :: Expr -> Expr
inline = foldId (In . Id . fmap defs)
  where
  defs (In (Id (Name n _))) = var n
  defs e                    = e

-- Collect all definitions from an expression tree and return a map with
-- name/definition pairs. Because of the Map datatype all duplicate definitions
-- will be joined to a single one.

collect :: Expr -> DefTable Id
collect = reduce defs
  where
  defs d@(Name n _) = [(n, In (Id d))]
  defs _            = []

-- Lift all definitions to the top-level and inline all references to these
-- definitions in the main expression.

lift :: Arrow (~>) => Expr ~> Definitions
lift = arr (uncurry Defs . (collect &&& inline))

dump :: Arrow (~>) => Definitions ~> String
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

    rec = tr . unId . out

