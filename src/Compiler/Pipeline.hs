{-# LANGUAGE Arrows #-}
module Compiler.Pipeline where

import Compiler.FreeVariables
import Compiler.Instantiate
import Compiler.LiftDefinitions
-- import Compiler.LiftLambdas
import Control.Arrow
import Lang.Value

compiler :: Val l i -> IO String
compiler = runKleisli
    $ instantiateLambas
  >>> liftDefinitions
  >>> annotateWithFreeVariables
--   >>> addLambdaAbstractions
--   >>> collectSuperCombinators
  >>> printDefinitionsWithFreeVariables

