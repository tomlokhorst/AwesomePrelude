module Compiler.Compiler where

import Compiler.CodeGen
import Compiler.Instantiate
import Compiler.LambdaLifting
import Compiler.Renamer
import Compiler.CSE
import Control.Arrow
import Control.Category
import Lang.JavaScript
import Prelude hiding ((.), id)

compiler :: Js a -> IO String
compiler =
  runKleisli
  $ concatDefinitions
  . generateCodeDefinitions
  . renameNamedDefinitions
  . commonSubExpressionElimination
  . liftLambdas
  . instantiateLambas

