module Compiler.LiftDefinitions 
( inlineDefinitions
, collectDefinitions
, liftDefinitions
, printDefinitions
)
where

import Compiler.Generics
import Compiler.Raw 
import Data.Map (Map, singleton, empty, elems)
import Control.Arrow hiding (app)
import Data.List (intercalate)

-- All named definitions within expression will be replaces by a variables with
-- the name of the definitions.

inlineDefinitions :: Expr -> Expr
inlineDefinitions = foldId (In . Id . fmap defs)
  where
  defs (In (Id (Def n _))) = var n
  defs e                   = e

-- Collect all definitions from an expression tree and return a map with
-- name/definition pairs. Because of the Map datatype all duplicate definitions
-- will be joined to a single one.

collectDefinitions :: Expr -> Map String Expr
collectDefinitions = reduce defs
  where
  defs d@(Def n _) = singleton n (In (Id d))
  defs _           = empty

-- Lift all definitions to the top-level and inline all references to these
-- definitions.

liftDefinitions :: Arrow (~>) => Expr ~> Expr
liftDefinitions = arr (\e -> more (elems (collectDefinitions e) ++ [def "__main" (tr e)]))
  where
    tr d@(In (Id (Def _ _))) = d
    tr e                     = inlineDefinitions e

printDefinitions :: Arrow (~>) => Expr ~> String
printDefinitions = arr tr
  where
    tr (In (Id (App   f e)))  = tr f ++ "(" ++ tr e ++ ")"
    tr (In (Id (Con   c)))    = c
    tr (In (Id (Prim  s _)))  = s
    tr (In (Id (Lam   as e))) = "(function (" ++ intercalate ", " as ++ ")" ++ "{ " ++ "return " ++ tr e ++ ";" ++ " })"
    tr (In (Id (Var   v)))    = v
    tr (In (Id (Def   n e)))  = n ++ " = " ++ tr e
    tr (In (Id (More  es)))   = intercalate "\n" (map tr es)

