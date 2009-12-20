{-# LANGUAGE Arrows #-}
module Compiler.Pipeline where

import Compiler.FreeVariables
import Compiler.Instantiate
import Compiler.LiftDefinitions
import Compiler.LiftClosures
-- import Compiler.LiftLambdas
import Control.Arrow
import Lang.Value

compiler :: Val l i -> IO String
compiler = runKleisli
    $ instantiateLambas
  >>> liftDefinitions
  >>> annotateWithFreeVariables
  >>> liftClosures
  >>> printDefinitions

--   >>> addLambdaAbstractions
--   >>> collectSuperCombinators
--   >>> printDefinitionsWithFreeVariables
--   >>> printAnonymousExpression
