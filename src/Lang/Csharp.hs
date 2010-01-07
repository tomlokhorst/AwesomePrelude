module Lang.Csharp where

import Lang.Value
import Generic.Control.Function
import Generic.Data.Bool
import Generic.Data.Either
import Generic.Data.List
import Generic.Data.Maybe
import Generic.Data.Number
import Generic.Data.Tuple
import qualified Prelude

data Csharp
type Cs a = Val Csharp a

instance Prelude.Show (Primitive Csharp) where
  show (Fun ys body) =
    case ys of
      []   -> body
      x:xs ->
        let b = if Prelude.null xs then body else Prelude.show (Fun xs body :: Primitive Csharp)
            cc = (Prelude.++)
        in x `cc` " => " `cc` b

-- * Csharp instances for AwesomePrelude 'data types'

instance FunC (Val Csharp) where
  lam f   = "lam" `Name` Lam f
  app f g = "app" `Name` App f g
  fix f   = "fix" `Name` fun1 ["f", ""] "f(fix(f))" (lam f)

instance BoolC (Val Csharp) where
  true       = "true"  `Name` con "true"
  false      = "fales" `Name` con "false"
  bool t e b = "bool"  `Name` fun3 ["t", "e", "b"] "b ? t : e" t e b

instance NumC (Val Csharp) where
  a + b = "add" `Name` fun2 ["a", "b"] "a + b" a b
  a - b = "sub" `Name` fun2 ["a", "b"] "a - b" a b
  a * b = "mul" `Name` fun2 ["a", "b"] "a * b" a b
  a / b = "div" `Name` fun2 ["a", "b"] "a / b" a b
  num x = con (Prelude.show x)

instance TupleC (Val Csharp) where
  mkTuple a b = "mkTuple" `Name` fun2 ["a", "b"] "new Tuple(a, b)" a b
  tuple f t = "tuple" `Name` fun2 ["f", "t"] "f(t.Item1, t.Item2)" (lam2 f) t

instance MaybeC (Val Csharp) where
  nothing     = "nothing" `Name` con "new Nothing()"
  just x      = "just" `Name` fun1 ["x"] "new Just(x)" x
  maybe n j m = "maybe" `Name` fun3 ["n", "j", "m"] "m.maybe(" n (lam j) m

instance EitherC (Val Csharp) where
  left l       = "left"   `Name` fun1 ["l"] "new Left(l)" l
  right r      = "right"  `Name` fun1 ["r"] "{ right : x }" r
  either l r e = "either" `Name` fun3 ["l", "r", "e"] "m.left ? l(x.left) : r(x.right)" (lam l) (lam r) e

instance ListC (Val Csharp) where
  nil         = "nil"  `Name` con "new Nil()"
  cons x xs   = "cons" `Name` fun2 ["x", "xs"] "new Cons(x, xs)" x xs
  list b f xs = "list" `Name` fun3 ["a", "f", "xs"] "xs.nil ? b : f(x.head, x.tail)" b (lam2 f) xs

-- * Csharp instances of AwesomePrelude 'type classes'

instance Eq (Val Csharp) Bool where
  a == b = "eq"  `Name` fun2 ["a", "b"] "a == b" a b
  a /= b = "neq" `Name` fun2 ["a", "b"] "a /= b" a b

instance (Eq (Val Csharp) a, Eq (Val Csharp) b) => Eq (Val Csharp) (a, b) where
  a == b = "eq"  `Name` fun2 ["a", "b"] "a == b" a b
  a /= b = "neq" `Name` fun2 ["a", "b"] "a /= b" a b

instance Eq (Val Csharp) a => Eq (Val Csharp) [a] where
  a == b = "eq"  `Name` fun2 ["a", "b"] "a == b" a b
  a /= b = "neq" `Name` fun2 ["a", "b"] "a /= b" a b

