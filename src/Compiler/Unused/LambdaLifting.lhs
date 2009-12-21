First, we will start of with the module header and some imports.

> module Compiler.LambdaLifting (liftLambdas) where

> import Compiler.Generics
> import Control.Arrow hiding (app)
> import Compiler.Expression
> import qualified Data.Set as S
> import Control.Monad.State

Lambda-lifting is done by doing three steps, as defined in 
<a href="http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.30.1125">A modular fully-lazy lambda lifter in Haskell</a>

Lambda-lifting gives us a list of definitions. The |Expression| datatype doesn't contain any |Abs| terms.

> liftLambdas :: Kleisli IO Expression Expression
> liftLambdas = arr (more . reverse . collectSCs . abstract . freeVars)

The |freeVars| function will annotate every expression with its variables. The type of such an annotated expression is:

> newtype AnnExpr a = AnnExpr {unAnn :: (a, ExprF (AnnExpr a))} -- deriving Show

These are some smart constructor/destructor functions:

> ae :: a -> ExprF (AnnExpr a) -> AnnExpr a
> ae a b = AnnExpr (a,b)

> fv :: AnnExpr a -> a
> fv = fst . unAnn

|freeVars| operates on simple fixpoints of |ExprF|:

> freeVars :: Expression -> AnnExpr (S.Set String)
> freeVars = freeVars' . out

|freeVars'| does the heavy lifting:

> freeVars' :: ExprF (Expression) -> AnnExpr (S.Set String)
> freeVars' (App l r)      =  let l' = freeVars l
>                                 r' = freeVars r
>                             in  ae (S.union (fv l') (fv r')) (App l' r')
> freeVars' (Con c)        =  ae S.empty (Con c)
> freeVars' (Prim s vs)    =  ae (S.fromList vs) (Prim s vs)
> freeVars' (Lam x expr)   =  let expr' = freeVars expr
>                             in  ae (S.difference (fv expr') (S.fromList x)) (Lam x expr')
> freeVars' (Var v)        =  ae (S.singleton v) (Var v)
> freeVars' (Def nm expr)  =  mapVal (Def nm) (freeVars expr)
> freeVars' (More _)       =  error "no idea"

> mapVal :: (AnnExpr t -> ExprF (AnnExpr t)) -> AnnExpr t -> AnnExpr t
> mapVal f (AnnExpr (a, e)) = ae a (f (AnnExpr (a, e)))

The function |abstract| changes every lambda expression |e| by adding
abstractions for all free variables in |e| (and an |App| as well).

> abstract :: AnnExpr (S.Set String) -> Expression
> abstract = f
>  where
>   f (AnnExpr (_, (App l r)))     = app (abstract l) (abstract r)
>   f (AnnExpr (_, (Con c)))       = con c
>   f (AnnExpr (_, (Prim s vs)))   = prim s vs -- TODO ???
>   f (AnnExpr (a, (Lam x expr)))  = let frees = S.toList a
>                                    in  addVars (In $ Lam (frees ++ x) (abstract expr)) frees
>   f (AnnExpr (_, (Var v)))       = var v
>   f (AnnExpr (_, (Def x expr)))  = def x (abstract expr)
>   f (AnnExpr (_, (More xs)))     = more (map f xs)

> addVars :: Expression -> [String] -> Expression
> addVars = foldl (\e -> app e . var)

The state could be changed into a |Reader| for the |freshVariables| and a |Writer| for the bindings.

> data CollectState = CollectState 
>   { freshVariable :: Int
>   , bindings :: [Expression]
>   }

collectSCs lifts all the lambdas to supercombinators (as described in the paper).

> collectSCs :: Expression -> [Expression]
> collectSCs e = let (e', st) = runState (collectSCs' $ out e) (CollectState 0 [])
>                in  (In (Def "main" e')):(bindings st)

> collectSCs' :: ExprF (Expression) -> State CollectState (Expression)
> collectSCs' (App l r)      = do l' <- collectSCs' (out l)
>                                 r' <- collectSCs' (out r)
>                                 return (app l' r')
> collectSCs' (Con c)        = do return (con c) -- nm <- freshName          -- to indirect
>     --                            write nm (In $ Con c)
>       --                          return $ In $ Var nm
> collectSCs' (Prim s vs)    = do return (prim s vs) -- nm <- freshName          -- to indirect
>                           --      write nm (In $ Prim s)
>                             --  return $ In $ Var nm
> collectSCs' (Lam x expr)   = do expr' <- collectSCs' (out expr)
>                                 nm <- freshName
>                                 write nm (In $ Lam x expr')
>                                 return $ In $ Var nm
> collectSCs' (Var v)        = return $ In (Var v)
> collectSCs' (Def nm expr)  = do expr' <- collectSCs' (out expr)
>                                 write nm expr'
>                                 return (In $ Var nm)
> collectSCs' (More _)       = error "collectSCs: More not supported yet."

Some helper functions to deal with state

> write :: String -> Expression -> State CollectState ()
> write nm expr = modify (\st -> st {bindings = (In (Def nm expr)):(bindings st)})

> freshName :: State CollectState String
> freshName = do st <- get
>                put (st {freshVariable = freshVariable st + 1})
>                return $ "__super__" ++ (show $ freshVariable st)

\begin{spec}
example = lam "x" (app (lam "y" (var "x")) (var "x"))
\end{spec}
