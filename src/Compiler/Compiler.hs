module Compiler.Compiler where

import Compiler.CodeGen
import Compiler.Instantiate
import Compiler.LambdaLifting
import Compiler.Renamer
import Control.Arrow
import Control.Category
import Data.Reify
import Lang.JavaScript
import Prelude hiding ((.), id)

type Step a b = Kleisli IO a b

compiler :: Js a -> IO String
compiler =
  runKleisli
  $ concatDefinitions
  . generateCodeDefinitions
  . renameNamedDefinitions
  . Kleisli reifyGraph
  . liftLambdas
  . instantiateLambas

