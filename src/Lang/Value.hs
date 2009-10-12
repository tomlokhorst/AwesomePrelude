module Lang.Value where

type Parameter = String
type Body = String
data Primitive l = Fun [Parameter] Body

-- Values have an index for the language and an index for the type of value
-- being represented.

data Val l a where
  Prim :: Primitive l -> Val l a
  App  :: Val l (a -> b) -> Val l a -> Val l b
  Lam  :: (Val l a -> Val l b) -> Val l (a -> b)
  Var  :: Integer -> Val l a

con :: String -> Val l a
con = Prim . Fun []

fun1 :: [Parameter] -> String -> Val l a -> Val l b
fun1 p b c = Prim (Fun p b) `App` c

fun2 :: [Parameter] -> String -> Val l a -> Val l b -> Val l c
fun2 p b c d = Prim (Fun p b) `App` c `App` d

fun3 :: [Parameter] -> String -> Val l a -> Val l b -> Val l c -> Val l d
fun3 p b c d e = Prim (Fun p b) `App` c `App` d `App` e

