module Compiler.Instantiate (instantiateLambas) where

import Compiler.Raw 
import Control.Arrow hiding (app)
import Control.Applicative
import Control.Monad.Reader
import qualified Lang.Value as V

instantiateLambas :: Show (V.Primitive l) => Kleisli IO (V.Val l i) Expr
instantiateLambas = arr (flip runReader 0 . tr)

tr :: Show (V.Primitive l) => V.Val l i -> Reader Integer Expr
tr (V.App f a)  = app <$> tr f <*> tr a
tr (V.Prim s)   = pure (prim (show s))
tr (V.Lam f)    = local (+1) (ask >>= \r -> lam ['v':show r] <$> tr (f (V.Var r)))
tr (V.Var x)    = pure (var ('v':show x))
tr (V.Name x v) = name x <$> tr v

