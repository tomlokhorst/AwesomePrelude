{-# LANGUAGE Arrows #-}
module Compiler.Pipeline where

import Compiler.Instantiate
import Control.Arrow
import Lang.Value
import qualified Compiler.FreeVariables as FreeVariables
import qualified Compiler.LiftDefinitions as Definitions
-- import Compiler.LiftClosures
-- import Compiler.LiftLambdas

compiler :: Val l i -> IO String
compiler = runKleisli
    $ instantiateLambas
  >>> Definitions.lift
  >>> FreeVariables.annotateDefinitions
--   >>> liftClosures
  >>> FreeVariables.dump

--   >>> addLambdaAbstractions
--   >>> collectSuperCombinators
--   >>> printDefinitionsWithFreeVariables
--   >>> printAnonymousExpression
