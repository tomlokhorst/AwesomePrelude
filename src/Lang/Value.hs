module Lang.Value where

type Parameter = String
data Primitive = 
    Fun [Parameter] String
  | Con String

-- Values have an index for the language and an index for the type of value
-- being represented.

data Val l a where
  Prim :: Primitive -> Val l a
  App  :: Val lang (a -> b) -> Val l a -> Val l b
  Lam  :: (Val l a -> Val l b) -> Val l (a -> b)
  Var  :: Val l v

con :: String -> Val l a
con = Prim . Con

fun1 :: [Parameter] -> String -> Val l a -> Val l b
fun1 p b c = Prim (Fun p b) `App` c

fun2 :: [Parameter] -> String -> Val l a -> Val l b -> Val l c
fun2 p b c d = Prim (Fun p b) `App` c `App` d

fun3 :: [Parameter] -> String -> Val l a -> Val l b -> Val l c -> Val l d
fun3 p b c d e = Prim (Fun p b) `App` c `App` d `App` e

