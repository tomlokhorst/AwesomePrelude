module Compiler.CSE (commonSubExpressionElimination, reifyGraphA) where

import Compiler.Raw
import Control.Arrow
import Control.Category
import Data.Reify.Graph.CSE
import Prelude hiding ((.), id)
import qualified Data.Reify as R

reifyGraphA :: R.MuRef a => Kleisli IO a (R.Graph (R.DeRef a))
reifyGraphA = Kleisli R.reifyGraph

commonSubExpressionElimination :: Kleisli IO Expr (R.Graph ExprF)
commonSubExpressionElimination = arr cse . Kleisli R.reifyGraph

-- MuRef instances for Data.Reify.

instance Traversable f => R.MuRef (FixA Id f) where
  type R.DeRef (Fix f) = f
  mapDeRef f = traverse f . unId . out

data Graph = Graph
  { nodes :: Map String (ExprF String)
  , first :: String
  }

