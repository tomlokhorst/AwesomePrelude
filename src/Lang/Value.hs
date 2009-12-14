{-# LANGUAGE GADTs #-}
module Lang.Value where

type Name = String
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
  Name :: String -> Val l a -> Val l a

con :: String -> Val l a
con = Prim . Fun []

fun1 :: String -> [Parameter] -> Body -> Val l a -> Val l b
fun1 n p b c = (n `Name` Prim (Fun p b)) `App` c

fun2 :: String -> [Parameter] -> Body -> Val l a -> Val l b -> Val l c
fun2 n p b c d = (n `Name` Prim (Fun p b)) `App` c `App` d

fun3 :: String -> [Parameter] -> Body -> Val l a -> Val l b -> Val l c -> Val l d
fun3 n p b c d e = (n `Name` Prim (Fun p b)) `App` c `App` d `App` e

