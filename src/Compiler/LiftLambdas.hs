module Compiler.LiftLambdas
( addLambdaAbstractions
, collectSuperCombinators
)
where

import Control.Monad.State
import Control.Applicative
import Compiler.Generics
import Compiler.Expr
import Compiler.FreeVariables
import Control.Arrow hiding (app)
import qualified Data.Set as S

-- The function |addLambdaAbstractions| changes every lambda expression |e| by
-- adding abstractions for all free variables in |e| (and an |App| as well).

addLambdaAbstractions :: Arrow (~>) => ExprFV ~> Expr
addLambdaAbstractions = arr ab
  where
  ab (In (FreeVarA _ (App l r)))   = app (ab l) (ab r)
  ab (In (FreeVarA _ (Con c)))     = con c
  ab (In (FreeVarA _ (Prim s vs))) = prim s vs
  ab (In (FreeVarA a (Lam x e)))   = let frees = S.toList a in addVars (lam (frees ++ x) (ab e)) frees
  ab (In (FreeVarA _ (Var v)))     = var v
  ab (In (FreeVarA _ (Def x e)))   = def x (addLambdaAbstractions e)
  ab (In (FreeVarA _ (More xs)))   = more (map ab xs)

  addVars = foldl (\e -> app e . var)

-- The state could be changed into a |Reader| for the |freshVariables| and a
-- |Writer| for the bindings.

data CollectState = CollectState 
  { freshVariable :: Int
  , bindings      :: [Expr]
  }

-- |collectSuperCombinators| lifts all the lambdas to supercombinators (as
-- described in the paper).

collectSuperCombinators :: Arrow (~>) => Expr ~> Expr
collectSuperCombinators = arr $ \ex ->
  let (ex', st) = runState (rec ex) (CollectState 0 [])
  in more (def "main" ex' : bindings st)

  where
  coll (App l r)   = app <$> rec l <*> rec r
  coll (Con c)     = return (con c)
  coll (Prim s vs) = return (prim s vs)
  coll (Lam x e)   = do e' <- rec e
                        nm <- freshName
                        write nm (lam x e')
                        return (var nm)
  coll (Var v)     = return (var v)
  coll (Def nm e)  = do rec e >>= write nm
                        return (var nm)
  coll (More xs)   = more <$> mapM rec xs

  rec = coll . unId . out

  write nm e = modify (\st -> st {bindings = bindings st ++ [def nm e]})

  freshName =
    do st <- get
       put (st {freshVariable = freshVariable st + 1})
       return $ "_s" ++ show (freshVariable st)

