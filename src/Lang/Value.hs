{-# LANGUAGE GADTs #-}
module Lang.Value where

-- Values have an index for the language and an index for the type of value
-- being represented.

data Val l a where
  Con  :: String -> Val l a
  Prim :: String -> [String] -> Val l a
  App  :: Val l (a -> b) -> Val l a -> Val l b
  Lam  :: (Val l a -> Val l b) -> Val l (a -> b)
  Var  :: String -> Val l a
  Name :: String -> Val l a -> Val l a

fun1 :: String -> (String -> String) -> Val l a -> Val l b
fun1 n f p = (n `Name` Lam (\(Var v) -> Prim (f v) [v])) `App` p

fun2 :: String -> (String -> String -> String) -> Val l a -> Val l b -> Val l c
fun2 n f p0 p1 = (n `Name` Lam (\(Var v) -> Lam (\(Var w) -> Prim (f v w) [v, w]))) `App` p0 `App` p1

fun3 :: String -> (String -> String -> String -> String) -> Val l a -> Val l b -> Val l c -> Val l d
fun3 n f p0 p1 p2 = (n `Name` Lam (\(Var v) -> Lam (\(Var w) -> Lam (\(Var x) -> Prim (f v w x) [v, w, x])))) `App` p0 `App` p1 `App` p2

