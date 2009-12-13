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
        in "function " `cc` " (" `cc` x `cc` ") { return " `cc` b `cc` " }"

-- * JavaScript instances for AwesomePrelude 'data types'

instance FunC (Val JavaScript) where
  lam f   = "lam" `Name` Lam f
  app f g = "app" `Name` App f g
  fix f   = "fix" `Name` fun1 ["f", ""] "f(fix(f))" (lam f)

instance BoolC (Val JavaScript) where
  true       = "true"  `Name` con "true"
  false      = "fales" `Name` con "false"
  bool t e b = "bool"  `Name` fun3 ["t", "e", "b"] "b ? t : e" t e b

instance NumC (Val JavaScript) where
  a + b = "add" `Name` fun2 ["a", "b"] "a + b" a b
  a - b = "sub" `Name` fun2 ["a", "b"] "a - b" a b
  a * b = "mul" `Name` fun2 ["a", "b"] "a * b" a b
  a / b = "div" `Name` fun2 ["a", "b"] "a / b" a b
  num x = con (Prelude.show x)

instance TupleC (Val JavaScript) where
  mkTuple a b = "mkTuple" `Name` fun2 ["a", "b"] "{ fst : a, snd : b}" a b
  tuple f t = "tuple" `Name` fun2 ["f", "t"] "f(t.fst, t.snd)" (lam2 f) t

instance MaybeC (Val JavaScript) where
  nothing     = "con" `Name` con "{ nothing : 1 }"
  just x      = "just" `Name` fun1 ["x"] "{ just : x }" x
  maybe n j m = "maybe" `Name` fun3 ["n", "j", "m"] "m.nothing ? n : j(m.just)" n (lam j) m

instance EitherC (Val JavaScript) where
  left l       = "left"   `Name` fun1 ["l"] "{ left  : x }" l
  right r      = "right"  `Name` fun1 ["r"] "{ right : x }" r
  either l r e = "either" `Name` fun3 ["l", "r", "e"] "m.left ? l(x.left) : r(x.right)" (lam l) (lam r) e

instance ListC (Val JavaScript) where
  nil         = "con"  `Name` con "{ nil : 1 }"
  cons x xs   = "cons" `Name` fun2 ["x", "xs"] "{ head : x, tail : xs }" x xs
  list b f xs = "list" `Name` fun3 ["a", "f", "xs"] "xs.nil ? b : f(x.head, x.tail)" b (lam2 f) xs

-- * JavaScript instances of AwesomePrelude 'type classes'

instance Eq (Val JavaScript) Bool where
  a == b = "eq"  `Name` fun2 ["a", "b"] "a == b" a b
  a /= b = "neq" `Name` fun2 ["a", "b"] "a /= b" a b

instance (Eq (Val JavaScript) a, Eq (Val JavaScript) b) => Eq (Val JavaScript) (a, b) where
  a == b = "eq"  `Name` fun2 ["a", "b"] "a == b" a b
  a /= b = "neq" `Name` fun2 ["a", "b"] "a /= b" a b

instance Eq (Val JavaScript) a => Eq (Val JavaScript) [a] where
  a == b = "eq"  `Name` fun2 ["a", "b"] "a == b" a b
  a /= b = "neq" `Name` fun2 ["a", "b"] "a /= b" a b

