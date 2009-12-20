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
lift = arr (Defs . concat . zipWith single [0..] . unDefs)

  where
  single i (Def n x) = let (g, (_, ds)) = runState (coll True x) (i, []) in ds ++ [Def n g]

    where
    coll top (In (FreeVarA vf e)) =
      if not top && S.size vf == 0 && liftable e
        then var . mk <$> (rec e >>= store)
        else rec e

    rec = fmap (In . Id) . traverse (coll False)

    liftable (App _ _) = True
    liftable (Lam _ _) = True
    liftable _         = False

    mk v = 'c': (show i ++ "_" ++ show v)

    store :: Expr -> State (Integer, [Definition]) Integer
    store e =
      do modify $ \(j, defs) -> (j + 1, defs ++ [Def (mk (j + 1)) e])
         gets fst

