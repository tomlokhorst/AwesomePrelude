module Lang.JavaScript where

import Generic.Control.Function
import Generic.Data.Bool
import Generic.Data.Either
import Generic.Data.List
import Generic.Data.Maybe
import Generic.Data.Number
import Generic.Data.Tuple
import qualified Prelude as P

type Parameter = P.String
data Primitive = 
    Fun [Parameter] P.String
  | Con P.String

data Js a where
  Prim :: Primitive -> Js a
  App  :: Js (a -> b) -> Js a -> Js b
  Lam  :: (Js a -> Js b) -> Js (a -> b)
  Var  :: Js v

con :: P.String -> Js a
con = Prim P.. Con

fun1 :: [Parameter] -> P.String -> Js a -> Js b
fun1 p b = app (Prim (Fun p b))

fun2 :: [Parameter] -> P.String -> Js a -> Js b -> Js c
fun2 p b = app2 (Prim (Fun p b))

fun3 :: [Parameter] -> P.String -> Js a -> Js b -> Js c -> Js d
fun3 p b = app3 (Prim (Fun p b))

instance FunC Js where
  lam = Lam
  app = App
  fix f = Prim (Fun ["f"] "fix") `app` lam f

-- * JavaScript instances for AwesomePrelude 'data types'

instance BoolC Js where
  true  = con "true"
  false = con "false"
  bool = fun3 ["t", "e", "b"] "b ? t : e"

instance NumC Js where
  (+) = fun2 ["a", "b"] "a + b"
  (-) = fun2 ["a", "b"] "a + b"
  (*) = fun2 ["a", "b"] "a + b"
  (/) = fun2 ["a", "b"] "a + b"
  num = Prim P.. Con P.. P.show

instance TupleC Js where
  mkTuple = fun2 ["a", "b"] "{ fst : a, snd : b}"
  tuple f = (fun2 ["f", "t"] "f(t.fst, t.snd)") (lam2 f)

instance MaybeC Js where
  nothing   = Prim (Con "{ nothing : 1 }")
  just      = fun1 ["x"] "{ just : x }"
  maybe n j = (fun3 ["n", "j", "m"] "m.nothing ? n : j(x.just)") n (lam j)

instance EitherC Js where
  left       = fun1 ["l"] "{ left  : x }"
  right      = fun1 ["r"] "{ right : x }"
  either l r = (fun3 ["l", "r", "e"] "m.left ? l(x.left) : r(x.right)") (lam l) (lam r)

instance ListC Js where
  nil      = con "{ nil : 1 }"
  cons     = fun2 ["x", "xs"] "{ head : x, tail : xs }"
  list b f = (fun3 ["a", "f", "xs"] "xs.nil ? b f(x.head, x.tail)") b (lam2 f)

-- * JavaScript instances of AwesomePrelude 'type classes'

instance Eq Js Bool where
  (==) = fun2 ["a", "b"] "a == b"
  (/=) = fun2 ["a", "b"] "a == b"

instance (Eq Js a, Eq Js b) => Eq Js (a, b) where
  (==) = fun2 ["a", "b"] "a == b"
  (/=) = fun2 ["a", "b"] "a == b"

instance Eq Js a => Eq Js [a] where
  (==) = fun2 ["a", "b"] "a == b"
  (/=) = fun2 ["a", "b"] "a == b"

