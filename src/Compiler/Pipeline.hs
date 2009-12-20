{-# LANGUAGE Arrows #-}
module Compiler.Pipeline where

import Control.Arrow
import Lang.Value (Val)
import qualified Compiler.InstantiateLambdas     as Lambdas
import qualified Compiler.FreeVariables          as FreeVariables
import qualified Compiler.LiftClosedApplications as ClosedApplications
import qualified Compiler.LiftDefinitions        as Definitions

compiler :: Val l i -> IO String
compiler = runKleisli
    $ Lambdas.instantiate
  >>> Definitions.lift
  >>> FreeVariables.annotateDefinitions
  >>> ClosedApplications.lift
  >>> Definitions.dump

