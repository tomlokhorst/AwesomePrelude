{-# LANGUAGE Arrows #-}
module Compiler.Pipeline where

import Control.Arrow
import Lang.Value (Val)
import Compiler.Expr
import Compiler.FreeVariables (DefinitionsFV)
import Compiler.LiftDefinitions (Definitions)
import qualified Compiler.InstantiateLambdas     as Lambdas
import qualified Compiler.FreeVariables          as FreeVariables
import qualified Compiler.LiftClosedApplications as ClosedApplications
import qualified Compiler.LiftDefinitions        as Definitions

type a :-> b = Kleisli IO a b

compiler :: Val l i -> IO String
compiler = runKleisli
    $ ( Lambdas.instantiate               :: Val l i       :-> Expr          )
  >>> ( Definitions.lift                  :: Expr          :-> Definitions   )
  >>> ( Definitions.eliminiateDoubles     :: Definitions   :-> Definitions   )
  >>> ( FreeVariables.annotateDefinitions :: Definitions   :-> DefinitionsFV )
  >>> ( ClosedApplications.lift           :: DefinitionsFV :-> Definitions   )
  >>> ( Definitions.dump                  :: Definitions   :-> String        )

