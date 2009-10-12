module Lang.JavaScript where

import Lang.Value
import Generic.Control.Function
import Generic.Data.Bool
import Generic.Data.Either
import Generic.Data.List
import Generic.Data.Maybe
import Generic.Data.Number
import Generic.Data.Tuple
import qualified Prelude

data JavaScript
type Js a = Val JavaScript a

instance Prelude.Show (Primitive JavaScript) where
  show = showJsPrim

cc :: [a] -> [a] -> [a]
cc = (Prelude.++)

showJsPrim :: Primitive JavaScript -> Prelude.String
showJsPrim (Fun []     body) = body
showJsPrim (Fun (x:xs) body) = "function (" `cc` x `cc` ") { return " `cc` b `cc` "}"
  where b = if Prelude.null xs then body else showJsPrim (Fun xs body)

-- * JavaScript instances for AwesomePrelude 'data types'

instance FunC (Val JavaScript) where
  lam = Lam
  app = App
  fix f = Prim (Fun ["f"] "f(arguments.callee(f))") `app` lam f

instance BoolC (Val JavaScript) where
  true  = con "true"
  false = con "false"
  bool  = fun3 ["t", "e", "b"] "b ? t : e"

instance NumC (Val JavaScript) where
  (+) = fun2 ["a", "b"] "a + b"
  (-) = fun2 ["a", "b"] "a - b"
  (*) = fun2 ["a", "b"] "a * b"
  (/) = fun2 ["a", "b"] "a / b"
  num x = con (Prelude.show x)

instance TupleC (Val JavaScript) where
  mkTuple = fun2 ["a", "b"] "{ fst : a, snd : b}"
  tuple f = (fun2 ["f", "t"] "f(t.fst, t.snd)") (lam2 f)

instance MaybeC (Val JavaScript) where
  nothing   = con "{ nothing : 1 }"
  just      = fun1 ["x"] "{ just : x }"
  maybe n j = (fun3 ["n", "j", "m"] "m.nothing ? n : j(x.just)") n (lam j)

instance EitherC (Val JavaScript) where
  left       = fun1 ["l"] "{ left  : x }"
  right      = fun1 ["r"] "{ right : x }"
  either l r = (fun3 ["l", "r", "e"] "m.left ? l(x.left) : r(x.right)") (lam l) (lam r)

instance ListC (Val JavaScript) where
  nil      = con "{ nil : 1 }"
  cons     = fun2 ["x", "xs"] "{ head : x, tail : xs }"
  list b f = (fun3 ["a", "f", "xs"] "xs.nil ? b : f(x.head, x.tail)") b (lam2 f)

-- * JavaScript instances of AwesomePrelude 'type classes'

instance Eq (Val JavaScript) Bool where
  (==) = fun2 ["a", "b"] "a == b"
  (/=) = fun2 ["a", "b"] "a /= b"

instance (Eq (Val JavaScript) a, Eq (Val JavaScript) b) => Eq (Val JavaScript) (a, b) where
  (==) = fun2 ["a", "b"] "a == b"
  (/=) = fun2 ["a", "b"] "a /= b"

instance Eq (Val JavaScript) a => Eq (Val JavaScript) [a] where
  (==) = fun2 ["a", "b"] "a == b"
  (/=) = fun2 ["a", "b"] "a /= b"

