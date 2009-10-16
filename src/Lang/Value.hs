module Lang.Value where

type Name = String
type Parameter = String
type Body = String
data Primitive l = Fun Name [Parameter] Body

-- Values have an index for the language and an index for the type of value
-- being represented.

data Val l a where
  Prim :: Primitive l -> Val l a
  App  :: Val l (a -> b) -> Val l a -> Val l b
  Lam  :: (Val l a -> Val l b) -> Val l (a -> b)
  Var  :: Integer -> Val l a

con :: String -> Val l a
con = Prim . Fun "" []

fun1 :: Name -> [Parameter] -> Body -> Val l a -> Val l b
fun1 n p b c = Prim (Fun n p b) `App` c

fun2 :: Name -> [Parameter] -> Body -> Val l a -> Val l b -> Val l c
fun2 n p b c d = Prim (Fun n p b) `App` c `App` d

fun3 :: Name -> [Parameter] -> Body -> Val l a -> Val l b -> Val l c -> Val l d
fun3 n p b c d e = Prim (Fun n p b) `App` c `App` d `App` e

