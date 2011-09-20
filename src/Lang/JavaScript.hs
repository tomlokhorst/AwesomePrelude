{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Lang.JavaScript where

import Prelude ((++))
import qualified Prelude as P

import Generic.Prelude hiding ((++))
import Lang.Value

data JS
type JavaScript a = Val JS a


-- * JavaScript instances for AwesomePrelude 'data types'.

instance NameC (Val JS) where
  named s a = s `Name` a

instance LamFunC (Val JS) where
  lam f   = Lam f

instance AppFunC (Val JS) where
  app f x = App f x

instance RecFunC (Val JS) where
  fix f   = fun1 "fix" (fun1' (\v -> "fix = arguments.callee, " ++ v ++ "(function (i) { return fix(" ++ v ++ ")(i) })")) (lam f)

instance BoolC (Val JS) where
  false      = Con "false"
  true       = Con "true"
  bool x y z = fun3 "bool" (fun3' (\f t b -> b ++ " ? " ++ t ++ "(/*force*/) : " ++ f ++ "(/*force*/)")) (lam (const x)) (lam (const y)) z

instance MaybeC (Val JS) where
  nothing   = Con "{ nothing : 1 }"
  just      = fun1 "just" (fun1' (\x -> "{ just : " ++ x ++ " }"))
  maybe p q = fun3 "maybe" (fun3' (\n j m -> m ++ ".nothing ? " ++ n ++ " : " ++ j ++ "(" ++ m ++ ".just)")) p (lam q)

instance TupleC (Val JS) where
  mkTuple   = fun2 "mkTuple" (fun2' (\a b -> "{ fst : " ++ a ++ ", snd : " ++ b ++ "}"))
  tuple p q = fun2 "tuple"   (fun2' (\f t -> f ++ "(" ++ t ++ ".fst, " ++ t ++ ".snd)")) (lam2 p) q

instance EitherC (Val JS) where
  left       = fun1 "left"   (fun1' (\l -> "{ left  : " ++ l ++ " }"))
  right      = fun1 "right"  (fun1' (\r -> "{ right : " ++ r ++ " }"))
  either p q = fun3 "either" (fun3' (\l r e -> e ++ ".left ? " ++ l ++ "(" ++ e ++ ".left) : " ++ r ++ "(" ++ e ++ ".right)")) (lam p) (lam q)

instance ListC (Val JS) where
  nil         = Con "{ nil : 1 }"
  cons        = fun2 "cons" (fun2' (\x xs -> "{ head : " ++ x ++ ", tail : " ++ xs ++ " }"))
  list b f    = fun3 "list" (fun3' (\n c xs -> xs ++ ".nil ? " ++ n ++ " : " ++ c ++ "(" ++ xs ++ ".head)(" ++ xs ++ ".tail)")) b (lam2 f)


-- * JavaScript instances of AwesomePrelude type classes.

data Number

instance Num (Val JS) Number where
  (+) = fun2 "add" (op " + ")
  (-) = fun2 "sub" (op " - ")
  (*) = fun2 "mul" (op " * ")
  fromInteger x = Con (P.show x)

instance Eq (Val JS) Bool where
  (==) = fun2 "eq"  (op " == ")
  (/=) = fun2 "neq" (op " /= ")

instance Eq (Val JS) Number where
  (==) = fun2 "eq"  (op " == ")
  (/=) = fun2 "neq" (op " /= ")

instance (Eq (Val JS) a, Eq (Val JS) b) => Eq (Val JS) (a, b) where
  (==) = fun2 "eq"  (op " == ")
  (/=) = fun2 "neq" (op " /= ")

instance Eq (Val JS) a => Eq (Val JS) [a] where
  (==) = fun2 "eq"  (op " == ")
  (/=) = fun2 "neq" (op " /= ")

op :: P.String -> [P.String] -> P.String
op s = fun2' (\x y -> x ++ s ++ y)

fun1' :: (P.String -> P.String) -> [P.String] -> P.String
fun1' f [x] = f x
fun1' _ _   = P.error "Lang.JavaScript.fun1': wrong number of arguments"

fun2' :: (P.String -> P.String -> P.String) -> [P.String] -> P.String
fun2' f [x, y] = f x y
fun2' _ _      = P.error "Lang.JavaScript.fun2': wrong number of arguments"

fun3' :: (P.String -> P.String -> P.String -> P.String) -> [P.String] -> P.String
fun3' f [x, y, z] = f x y z
fun3' _ _         = P.error "Lang.JavaScript.fun3': wrong number of arguments"

