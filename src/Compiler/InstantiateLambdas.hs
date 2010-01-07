{-# LANGUAGE GADTs, TypeOperators #-}

module Compiler.InstantiateLambdas (instantiate, dump) where

import Compiler.Generics
import Compiler.Expression 
import Control.Applicative
import Control.Arrow hiding (app)
import Control.Monad.Reader
import Data.List (intercalate)
import qualified Lang.Value as V

instantiate :: Arrow (~>) => V.Val l i ~> Expression
instantiate = arr (flip runReader 0 . tr)
  where
    tr :: V.Val l i -> Reader Integer Expression
    tr (V.App  f a ) = app <$> tr f <*> tr a
    tr (V.Con  c   ) = pure (con c)
    tr (V.Lam  f   ) = local (+1) (ask >>= \r -> let v = 'v':show r in lam [v] <$> tr (f (V.Var v)))
    tr (V.Name n e ) = name n <$> tr e
    tr (V.Prim s vs) = pure (prim s vs)
    tr (V.Var  v   ) = pure (var v)

dump :: Arrow (~>) => Expression ~> String
dump = arr rec
  where
    tr (App  f e ) = rec f ++ "(\n" ++ indent (rec e) ++ ")"
    tr (Con  c   ) = c
    tr (Lam  as e) = "(function (" ++ intercalate ", " as ++ ")" ++ "\n{\n" ++ indent ("return " ++ rec e ++ ";") ++ "\n})"
    tr (Name n e ) = "/* " ++ n ++ "*/ " ++ rec e
    tr (Prim s vs) = s vs ++ " /* free: " ++ intercalate ", " vs ++ " */"
    tr (Var  v   ) = v

    rec = tr . unId . out
    indent = unlines . map ("  "++) . lines

