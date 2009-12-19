module Lang.JavaScript where

import Generic.Control.Function
import Generic.Data.Bool
import Generic.Data.Either
import Generic.Data.List hiding ((++))
import Generic.Data.Maybe
import Generic.Data.Number
import Generic.Data.Tuple
import Lang.Value
import Prelude ((++))
import qualified Prelude

data JavaScript
type Js a = Val JavaScript a

-- * JavaScript instances for AwesomePrelude datatypes.

instance NameC (Val JavaScript) where
  named s a = s `Name` a

instance FunC (Val JavaScript) where
  lam f   = Lam f
  app f g = App f g
  fix f   = fun1 "fix" (\v -> "fix = arguments.callee, " ++ v ++ "(function (i) { return fix(" ++ v ++ ")(i) })") (lam f)

instance BoolC (Val JavaScript) where
  true  = Con "true"
  false = Con "false"
  bool  = fun3 "bool" (\t e b -> b ++ " ? " ++ t ++ " : " ++ e)

instance NumC (Val JavaScript) where
  (+) = fun2 "add" (\a b -> a ++ " + " ++ b)
  (-) = fun2 "sub" (\a b -> a ++ " - " ++ b)
  (*) = fun2 "mul" (\a b -> a ++ " * " ++ b)
  (/) = fun2 "div" (\a b -> a ++ " / " ++ b)
  num x = Con (Prelude.show x)

instance MaybeC (Val JavaScript) where
  nothing   = Con "{ nothing : 1 }"
  just      = fun1 "just" (\x -> "{ just : " ++ x ++ " }")
  maybe p q = fun3 "maybe" (\n j m -> m ++ ".nothing ? " ++ n ++ " : " ++ j ++ "(" ++ m ++ ".just)") p (lam q)

instance TupleC (Val JavaScript) where
  mkTuple   = fun2 "mkTuple" (\a b -> "{ fst : " ++ a ++ ", snd : " ++ b ++ "}")
  tuple p q = fun2 "tuple"   (\f t -> f ++ "(" ++ t ++ ".fst, " ++ t ++ ".snd)") (lam2 p) q

instance EitherC (Val JavaScript) where
  left       = fun1 "left"   (\l -> "{ left  : " ++ l ++ " }")
  right      = fun1 "right"  (\r -> "{ right : " ++ r ++ " }")
  either p q = fun3 "either" (\l r e -> e ++ ".left ? " ++ l ++ "(" ++ e ++ ".left) : " ++ r ++ "(" ++ e ++ ".right)") (lam p) (lam q)

instance ListC (Val JavaScript) where
  nil         = Con "{ nil : 1 }"
  cons        = fun2 "cons" (\x xs -> "{ head : " ++ x ++ ", tail : " ++ xs ++ " }")
  list b f    = fun3 "list" (\n c xs -> xs ++ ".nil ? " ++ n ++ " : " ++ c ++ "(" ++ xs ++ ".head)(" ++ xs ++ ".tail)") b (lam2 f)

-- * JavaScript instances of AwesomePrelude type classes.

instance Eq (Val JavaScript) Bool where
  (==) = fun2 "eq"  (\a b -> a ++ " == " ++ b)
  (/=) = fun2 "neq" (\a b -> a ++ " /= " ++ b)

instance (Eq (Val JavaScript) a, Eq (Val JavaScript) b) => Eq (Val JavaScript) (a, b) where
  (==) = fun2 "eq"  (\a b -> a ++ " == " ++ b)
  (/=) = fun2 "neq" (\a b -> a ++ " /= " ++ b)

instance Eq (Val JavaScript) a => Eq (Val JavaScript) [a] where
  (==) = fun2 "eq"  (\a b -> a ++ " == " ++ b)
  (/=) = fun2 "neq" (\a b -> a ++ " /= " ++ b)

