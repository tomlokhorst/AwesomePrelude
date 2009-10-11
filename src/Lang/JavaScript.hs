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

instance Fun Js where
  lam = Lam
  ($) = App
  fix f = Prim (Fun ["f"] "fix") $ lam f

prim1 :: Primitive -> Js a -> Js b
prim1 f x = Prim f # x

prim2 :: Primitive -> Js a -> Js b -> Js c
prim2 f x y = prim1 f x # y

prim3 :: Primitive -> Js a -> Js b -> Js c -> Js d
prim3 f x y z = prim2 f x y # z

-- * JavaScript instances for AwesomePrelude 'data types'

instance BoolC Js where
  true  = Prim (Con "true")
  false = Prim (Con "false")
  bool = prim3 (Fun ["t", "e", "b"] "b ? t : e")

instance NumC Js where
  (+) = prim2 (Fun ["a", "b"] "a + b")
  (-) = prim2 (Fun ["a", "b"] "a + b")
  (*) = prim2 (Fun ["a", "b"] "a + b")
  (/) = prim2 (Fun ["a", "b"] "a + b")
  num = Prim P.. Con P.. P.show

instance TupleC Js where
  mkTuple = prim2 (Fun ["a", "b"] "{ fst : a, snd : b}")
  tuple f = prim2 (Fun ["f", "t"] "f(t.fst, t.snd)") (lam (lam P.. f))

instance MaybeC Js where
  nothing   = Prim (Con "{ nothing : 1 }")
  just      = prim1 (Fun ["x"] "{ just : x }")
  maybe n j = prim3 (Fun ["n", "j", "m"] "m.nothing ? n : j(x.just)") n (lam j)

instance EitherC Js where
  left       = prim1 (Fun ["l"] "{ left  : x }")
  right      = prim1 (Fun ["r"] "{ right : x }")
  either l r = prim3 (Fun ["l", "r", "e"] "m.left ? l(x.left) : r(x.right)") (lam l) (lam r)

instance ListC Js where
  nil      = Prim (Con "{ nil : 1 }")
  cons     = prim2 (Fun ["x", "xs"] "{ head : x, tail : xs }")
  list b f = prim3 (Fun ["a", "f", "xs"] "xs.nil ? b f(x.head, x.tail)") b (lam (lam P.. f))

-- * JavaScript instances of AwesomePrelude 'type classes'

instance Eq Js Bool where
  (==) = prim2 (Fun ["a", "b"] "a == b")
  (/=) = prim2 (Fun ["a", "b"] "a == b") 

instance (Eq Js a, Eq Js b) => Eq Js (a, b) where
  (==) = prim2 (Fun ["a", "b"] "a == b")
  (/=) = prim2 (Fun ["a", "b"] "a == b") 

instance Eq Js a => Eq Js [a] where
  (==) = prim2 (Fun ["a", "b"] "a == b")
  (/=) = prim2 (Fun ["a", "b"] "a == b") 

