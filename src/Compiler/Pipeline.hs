{-# LANGUAGE Arrows, TypeOperators #-}

module Compiler.Pipeline where

import Compiler.Expression
import Compiler.FreeVariables (DefinitionsFV)
import Compiler.LiftDefinitions (Definitions)
import Control.Arrow
import Lang.JavaScript
import qualified Compiler.CommonDefinitions      as CommonDefinitions
import qualified Compiler.FreeVariables          as FreeVariables
import qualified Compiler.InstantiateLambdas     as Lambdas
import qualified Compiler.LiftClosedApplications as ClosedApplications
import qualified Compiler.LiftDefinitions        as Definitions
import qualified Compiler.ReindexParamaters      as Parameters

type a :-> b = Kleisli IO a b

compiler :: JavaScript a -> IO String
compiler = runKleisli
    $ ( Lambdas.instantiate                  :: JavaScript a  :-> Expression    )
  >>> ( Definitions.lift                     :: Expression    :-> Definitions   )
  >>> ( Definitions.eliminiateDoubles        :: Definitions   :-> Definitions   )
  >>> ( FreeVariables.annotateDefinitions    :: Definitions   :-> DefinitionsFV )
  >>> ( ClosedApplications.lift              :: DefinitionsFV :-> Definitions   )
  >>> ( Parameters.reindex                   :: Definitions   :-> Definitions   )
  >>> ( CommonDefinitions.eliminate          :: Definitions   :-> Definitions   )
  >>> ( Definitions.dump                     :: Definitions   :-> String        )

