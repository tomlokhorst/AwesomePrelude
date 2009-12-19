module Compiler.LiftClosures where

import Control.Applicative
import Compiler.FreeVariables
import Compiler.Generics
import Compiler.Raw
import Control.Arrow hiding (app)
import Control.Monad.State
import Data.Traversable
import qualified Data.Set as S
import qualified Data.Map as M

liftClosures :: Arrow (~>) => ExprFV ~> Expr
liftClosures = arr (flip evalState (0, M.empty) . collectAll)

collectAll :: ExprFV -> State (Integer, M.Map Integer Expr) Expr
collectAll (In (FreeVarA vf x)) =
  if and [S.size vf == 0,  liftable x]
    then do s <- rec x >>= collectSingle
            return (var ('c': show s))
    else rec x
  where rec (Def n d) = do z <- collectAll d
                           mp <- gets snd
                           modify (\(a, _) -> (a, M.empty))
                           return (more (M.elems mp ++ [def n z]))
        rec e         = In . Id <$> traverse collectAll e

collectSingle :: Expr -> State (Integer, M.Map Integer Expr) Integer
collectSingle e =
  do modify $ \(i, store) ->
       let v = i + 1 :: Integer
       in (v, M.insert v (def ('c':show v) e) store)
     gets fst

liftable :: ExprF t -> Bool
liftable (App _ _) = True
liftable _         = False

