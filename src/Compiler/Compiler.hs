module Compiler.Compiler where

import Compiler.LambdaLifting
import Compiler.Raw
import Data.Reify
import qualified Lang.Value as Ix

pipeline :: Show (Ix.Primitive l) => Ix.Val l i -> IO String
pipeline inp = 
  do x <- reifyGraph . (In . More) . lambdaLift . raw $ inp
     return (unlines . valmapToDefinitions . renamer {-. CSE.cse-} $ x)


