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
  show (Fun ys body) =
    case ys of
      []   -> body
      x:xs ->
        let b = if Prelude.null xs then body else Prelude.show (Fun xs body :: Primitive JavaScript)
            cc = (Prelude.++)
        in "function " `cc` "(" `cc` x `cc` ") { return " `cc` b `cc` " }"

-- * JavaScript instances for AwesomePrelude 'data types'

instance NameC (Val JavaScript) where
  named s a = s `Name` a

instance FunC (Val JavaScript) where
  lam f   = Lam f
  app f g = App f g
  fix f   = fun1 "fix" ["f", ""] "f(fix(f))" (lam f)

instance BoolC (Val JavaScript) where
  true       = con "true"
  false      = con "false"
  bool t e b = fun3 "bool"   ["t", "e", "b"] "b ? t : e" t e b

instance NumC (Val JavaScript) where
  a + b = fun2 "add" ["a", "b"] "a + b" a b
  a - b = fun2 "sub" ["a", "b"] "a - b" a b
  a * b = fun2 "mul" ["a", "b"] "a * b" a b
  a / b = fun2 "div" ["a", "b"] "a / b" a b
  num x = con (Prelude.show x)

instance TupleC (Val JavaScript) where
  mkTuple a b = fun2 "mkTuple" ["a", "b"] "{ fst : a, snd : b}" a b
  tuple f t   = fun2 "tuple"   ["f", "t"] "f(t.fst, t.snd)" (lam2 f) t

instance MaybeC (Val JavaScript) where
  nothing     = con "{ nothing : 1 }"
  just x      = fun1 "just"  ["x"] "{ just : x }" x
  maybe n j m = fun3 "maybe" ["n", "j", "m"] "m.nothing ? n : j(m.just)" n (lam j) m

instance EitherC (Val JavaScript) where
  left l       = fun1 "left"   ["l"] "{ left  : x }" l
  right r      = fun1 "right"  ["r"] "{ right : x }" r
  either l r e = fun3 "either" ["l", "r", "e"] "m.left ? l(x.left) : r(x.right)" (lam l) (lam r) e

instance ListC (Val JavaScript) where
  nil         = con "{ nil : 1 }"
  cons x xs   = fun2 "cons" ["x", "xs"] "{ head : x, tail : xs }" x xs
  list b f xs = fun3 "list" ["a", "f", "xs"] "xs.nil ? b : f(x.head, x.tail)" b (lam2 f) xs

-- * JavaScript instances of AwesomePrelude 'type classes'

instance Eq (Val JavaScript) Bool where
  a == b = fun2 "eq"  ["a", "b"] "a == b" a b
  a /= b = fun2 "neq" ["a", "b"] "a /= b" a b

instance (Eq (Val JavaScript) a, Eq (Val JavaScript) b) => Eq (Val JavaScript) (a, b) where
  a == b = fun2 "eq"  ["a", "b"] "a == b" a b
  a /= b = fun2 "neq" ["a", "b"] "a /= b" a b

instance Eq (Val JavaScript) a => Eq (Val JavaScript) [a] where
  a == b = fun2 "eq"  ["a", "b"] "a == b" a b
  a /= b = fun2 "neq" ["a", "b"] "a /= b" a b

