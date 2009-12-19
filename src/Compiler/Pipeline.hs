{-# LANGUAGE Arrows #-}
module Compiler.Pipeline where

import Compiler.Instantiate
import Compiler.LiftDefinitions
import Control.Arrow
import Lang.Value

compiler :: Val l i -> IO String
compiler = runKleisli $ proc i ->
  do r <- instantiateLambas  -< i
     l <- liftDefinitions    -< r
     p <- definitionsPrinter -< l
     returnA -< p

