module Compiler.LiftClosedApplications (lift) where

import Compiler.FreeVariables (DefinitionsFV, FreeVarA (..))
import Compiler.Generics
import Compiler.LiftDefinitions (DefinitionA (..), DefinitionsA (..), Definition, Definitions)
import Compiler.Expr
import Control.Applicative
import Control.Arrow hiding (app)
import Control.Monad.State hiding (lift)
import Data.Traversable
import qualified Data.Set as S

lift :: Arrow (~>) => DefinitionsFV ~> Definitions
lift = arr (Defs . concatMap single . unDefs)

  where
  single (Def n e) = let (g, (_, ds)) = runState (coll e) (0, []) in ds ++ [Def n g]

  coll (In (FreeVarA vf e)) =
    if S.size vf == 0 && liftable e
      then var . mk <$> (rec e >>= store)
      else rec e

  rec = fmap (In . Id) . traverse coll

  liftable (App _ _) = True
  liftable _         = False

  mk = ('c':) . show

  store :: Expr -> State (Integer, [Definition]) Integer
  store e =
    do modify $ \(i, defs) -> (i + 1, defs ++ [Def (mk (i + 1)) e])
       gets fst

