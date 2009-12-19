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

