module Compiler.Instantiate (instantiateLambas, anonymousExprPrinter) where

import Compiler.Generics
import Compiler.Raw 
import Control.Applicative
import Control.Arrow hiding (app)
import Control.Monad.Reader
import Data.List (intercalate)
import qualified Lang.Value as V

instantiateLambas :: Arrow (~>) => V.Val l i ~> Expr
instantiateLambas = arr (flip runReader 0 . tr)
  where
    tr :: V.Val l i -> Reader Integer Expr
    tr (V.App  f a)  = app <$> tr f <*> tr a
    tr (V.Con  c)    = pure (con c)
    tr (V.Prim s vs) = pure (prim s vs)
    tr (V.Lam  f)    = local (+1) (ask >>= \r -> let v = 'v':show r in lam [v] <$> tr (f (V.Var v)))
    tr (V.Var  v)    = pure (var v)
    tr (V.Name n e)  = def n <$> tr e

anonymousExprPrinter :: Arrow (~>) => Expr ~> String
anonymousExprPrinter = arr tr
  where
    tr (In (Id (App   f e)))  = tr f ++ "(\n" ++ indent (tr e) ++ ")"
    tr (In (Id (Con   c)))    = c
    tr (In (Id (Prim  s vs))) = s ++ " /* free: " ++ intercalate ", " vs ++ " */"
    tr (In (Id (Lam   as e))) = "(function (" ++ intercalate ", " as ++ ")" ++ "\n{\n" ++ indent ("return " ++ tr e ++ ";") ++ "\n})"
    tr (In (Id (Var   v)))    = v
    tr (In (Id (Def   n e)))  = "/* " ++ n ++ "*/ " ++ tr e
    tr (In (Id (More  es)))   = intercalate "\n" (map tr es)

    indent = unlines . map ("  "++) . lines

