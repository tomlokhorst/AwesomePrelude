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
  show (Fun name ys body) =
    case ys of
      []   -> body
      x:xs ->
        let b = if Prelude.null xs then body else Prelude.show (Fun "" xs body :: Primitive JavaScript)
            cc = (Prelude.++)
        in "function " `cc` name `cc` " (" `cc` x `cc` ") { return " `cc` b `cc` " }"

-- * JavaScript instances for AwesomePrelude 'data types'

instance FunC (Val JavaScript) where
  lam = Lam
  app = App
  fix f = fun1 "fix" ["f", ""] "f(fix(f))" (lam f)

instance BoolC (Val JavaScript) where
  true  = con "true"
  false = con "false"
  bool  = fun3 "bool" ["t", "e", "b"] "b ? t : e"

instance NumC (Val JavaScript) where
  (+) = fun2 "add" ["a", "b"] "a + b"
  (-) = fun2 "sub" ["a", "b"] "a - b"
  (*) = fun2 "mul" ["a", "b"] "a * b"
  (/) = fun2 "div" ["a", "b"] "a / b"
  num x = con (Prelude.show x)

instance TupleC (Val JavaScript) where
  mkTuple = fun2 "mkTuple" ["a", "b"] "{ fst : a, snd : b}"
  tuple f = (fun2 "tuple" ["f", "t"] "f(t.fst, t.snd)") (lam2 f)

instance MaybeC (Val JavaScript) where
  nothing   = con "{ nothing : 1 }"
  just      = fun1 "just" ["x"] "{ just : x }"
  maybe n j = (fun3 "maybe" ["n", "j", "m"] "m.nothing ? n : j(m.just)") n (lam j)

instance EitherC (Val JavaScript) where
  left       = fun1 "left"  ["l"] "{ left  : x }"
  right      = fun1 "right" ["r"] "{ right : x }"
  either l r = (fun3 "either" ["l", "r", "e"] "m.left ? l(x.left) : r(x.right)") (lam l) (lam r)

instance ListC (Val JavaScript) where
  nil      = con "{ nil : 1 }"
  cons     = {- fun2 [] "cons" -} fun2 "cons" ["x", "xs"] "{ head : x, tail : xs }"
  list b f = {- fun3 [] "list" b (lam2 f) -} (fun3 "list" ["a", "f", "xs"] "xs.nil ? b : f(x.head, x.tail)") b (lam2 f)

-- * JavaScript instances of AwesomePrelude 'type classes'

instance Eq (Val JavaScript) Bool where
  (==) = fun2 "eq"  ["a", "b"] "a == b"
  (/=) = fun2 "neq" ["a", "b"] "a /= b"

instance (Eq (Val JavaScript) a, Eq (Val JavaScript) b) => Eq (Val JavaScript) (a, b) where
  (==) = fun2 "eq"  ["a", "b"] "a == b"
  (/=) = fun2 "neq" ["a", "b"] "a /= b"

instance Eq (Val JavaScript) a => Eq (Val JavaScript) [a] where
  (==) = fun2 "eq"  ["a", "b"] "a == b"
  (/=) = fun2 "neq" ["a", "b"] "a /= b"

