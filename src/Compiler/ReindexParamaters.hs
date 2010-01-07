{-# LANGUAGE TypeOperators #-}

module Compiler.ReindexParamaters where

import Compiler.Expression
import Compiler.Generics
import Compiler.LiftDefinitions
import Control.Applicative hiding (empty)
import Control.Arrow hiding (app)
import Control.Monad.State
import Data.Map (Map, insert, lookup, empty)
import Data.Maybe
import Prelude hiding (lookup)

reindex :: Arrow (~>) => Definitions ~> Definitions
reindex = arr (Defs . map one . unDefs)
  where

  one (Def nm x) = let e = fst $ runState (rec x) (0, empty) in Def nm e
    where

    tr :: ExpressionF Expression -> State (Integer, Map Var Integer) Expression
    tr (App  f e ) = app <$> rec f <*> rec e
    tr (Con  c   ) = return (con c)
    tr (Lam  ps e) = do qs <- mapM (\p -> modify (\(c, m) -> (c + 1, insert p (c + 1) m)) >> gets (mk . fst)) ps
                        lam qs <$> rec e
    tr (Name n e ) = name n <$> rec e
    tr (Prim s vs) = prim s <$> mapM subst vs
    tr (Var  v   ) = subst v >>= return . var

    rec = tr . unId . out
    mk = ('v':) . show
    subst v = gets (maybe v mk . lookup v . snd)

