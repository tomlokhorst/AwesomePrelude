{-# LANGUAGE Arrows #-}
module Compiler.Pipeline where

import Control.Arrow
import Lang.JavaScript
import Compiler.Expr
import Compiler.FreeVariables (DefinitionsFV)
import Compiler.LiftDefinitions (Definitions)
import qualified Compiler.InstantiateLambdas     as Lambdas
import qualified Compiler.FreeVariables          as FreeVariables
import qualified Compiler.LiftClosedApplications as ClosedApplications
import qualified Compiler.LiftDefinitions        as Definitions
import qualified Compiler.ReindexParamaters      as Parameters
import qualified Compiler.CommonDefinitions      as CommonDefinitions

type a :-> b = Kleisli IO a b

compiler :: JavaScript a -> IO String
compiler = runKleisli
    $ ( Lambdas.instantiate               :: JavaScript a  :-> Expr          )
  >>> ( Definitions.lift                  :: Expr          :-> Definitions   )
  >>> ( Definitions.eliminiateDoubles     :: Definitions   :-> Definitions   )
  >>> ( FreeVariables.annotateDefinitions :: Definitions   :-> DefinitionsFV )
  >>> ( ClosedApplications.lift           :: DefinitionsFV :-> Definitions   )
  >>> ( Parameters.reindex                :: Definitions   :-> Definitions   )
  >>> ( CommonDefinitions.eliminate       :: Definitions   :-> Definitions   )
  >>> ( Definitions.dump                  :: Definitions   :-> String        )

