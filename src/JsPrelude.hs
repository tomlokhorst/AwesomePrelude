{-# LANGUAGE
    GADTs
  , EmptyDataDecls
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , ScopedTypeVariables
 #-}

module JsPrelude where

import AwesomePrelude
import qualified Prelude as P
import Data.List
import Data.AwesomeList
import Control.Function

-- * JavaScript DSL

newtype Js1 f a   = Js1 { unJs1 :: Js (f (Js a)) }
newtype Js2 f a b = Js2 { unJs2 :: Js (f (Js a) (Js b)) }

type a :-> b = Js2 (->) a b

data Js a where
  Prim     :: P.String -> Js a
  App      :: Js (Js a -> Js b) -> Js a -> Js b
  Case     :: [(P.String, Js a)] -> Js b -> Js c
  BinOp    :: P.String -> Js a -> Js b -> Js c
  TriOp    :: P.String -> P.String -> Js a -> Js b -> Js c -> Js d
  Fix      :: (Js a -> Js a) -> Js a

-- instance Fun (Js2 (->)) where
--   id = Js2 $ Prim "function (x) { return x; }"
--   f $ x = undefined
--   (.) = undefined

prim :: P.String -> Js a -> Js b
prim f a = Prim f `App` a

prim2 :: P.String -> Js a -> Js b -> Js c 
prim2 f a b = Prim f `App` a `App` b

prim3 :: P.String -> Js a -> Js b -> Js c -> Js d 
prim3 f a b c = Prim f `App` a `App` b `App` c

instance P.Show (Js a) where
  show = showJs

showJs :: Js a -> P.String
showJs (Prim s)          = s
showJs (Case xs f)       = "(function rec(x) { return "
                             P.++ P.foldr (\(x, j) s -> x P.++ " ? " P.++ (P.show j) P.++ " : " P.++ s)
                                          "undefined" xs
                             P.++ " })(" P.++ P.show f P.++ ")"
showJs (BinOp s   a b)   = "(" P.++ showJs a P.++ " " P.++ s P.++ " " P.++ showJs b P.++ ")"
showJs (TriOp s t a b c) = "(" P.++ showJs a P.++ " " P.++ s P.++ " " P.++ showJs b P.++ " "
                             P.++ t P.++ " "P.++ showJs c P.++ ")"
showJs (Fix f)           = "(function fix(f) { return f(fix(f)); })" P.++ showJs (Fix f)
showJs p@(App _ _)       = fu p P.++ "(" P.++ intercalate "," (args p) P.++ ")"
  where
    fu :: Js a -> P.String
    fu (App f _) = fu f
    fu x         = showJs x

    args :: Js a -> [P.String]
    args (App f' a) = args f' P.++ [showJs a]
    args _          = []

instance P.Show (f a) => P.Show (Js1 f a) where
  show jc = P.show (unJs1 jc)

instance P.Show a => P.Show (JsMaybe a) where
  show ls = P.show ls

instance P.Show a => P.Show (JsList' a) where
  show ls = P.show ls

data JsBool
data JsNumber
data JsMaybe a
data JsEither a b
data JsTuple2 a b
data JsList' a
type JsList a = Js1 JsList' a

fun :: [P.String] -> P.String -> P.String
fun ps ret = "(function (" P.++ intercalate ", " ps P.++ "){ return " P.++ ret P.++ "})"

-- Terible hack to get numeric literals to work

instance P.Num (Js JsNumber) where
  (+)           = BinOp "+"
  (*)           = BinOp "*"
  (-)           = BinOp "-"
  abs           = prim (fun ["x"] "Math.abs(x)")
  signum        = prim (fun ["x"] "Math.sign(x)")
  fromInteger x = Prim (P.show x)

instance P.Eq (Js JsNumber) where
  (==) = P.error "Prelude.Eq shouldn't be used with the AwesomePrelude"
  (/=) = P.error "Prelude.Eq shouldn't be used with the AwesomePrelude"


-- * JavaScript instances for AwesomePrelude 'data types'

-- (Fix (Js1 JsList' (Js JsNumber) -> Js JsNumber))
instance Fix (Js1 f (Js a) -> Js a) where
  ---fix :: ((Js1 f a -> Js a) -> (Js1 f a -> js a))
  ---        -> (Js1 f a -> Js a)
  fix f = let x = fix (undefined . f . undefined) :: Js a
          in \x -> Prim (P.show (unJs1 x))

-- unJs1 :: Js1 f a -> Js (f (Js a))
-- 
-- fix :: (Js a -> Js a) -> Js a
-- fix :: (Js1 f a -> Js1 f a) -> Js1 f a
-- App :: Js (Js a -> Js b) -> Js a -> Js b

instance Fix (Js a) where
  fix = Fix

instance Fix (Js1 f a) where
  fix f = Js1 $ fix (unJs1 . f . Js1)

instance Fix (Js2 f a b) where
  fix f = Js2 $ fix (unJs2 . f . Js2)

instance BoolC (Js JsBool) where
  trueC       = Prim "true"
  falseC      = Prim "false"

instance BoolD (Js JsBool) (Js r) where
  boolD f t p = TriOp "?" ":" p t f

instance Bool (Js JsBool) (Js r) where
  bool f t p = TriOp "?" ":" p t f
  true       = Prim "true"
  false      = Prim "false"

instance MaybeC (Js1 JsMaybe) (Js a) where
  nothingC     = Js1 (Prim "\"nothing\"")
  justC x      = Js1 (prim (fun ["x"] "{ just : x }") x)

instance MaybeD (Js1 JsMaybe) (Js a) (Js r) where
  maybeD x f m = Case [("x.just !== undefined", f (Prim "x.just")), ("true", x)] (unJs1 m)

instance Maybe (Js1 JsMaybe) (Js a) (Js r) where
  maybe x f m = Case [("x.just !== undefined", f (Prim "x.just")), ("true", x)] (unJs1 m)
  nothing     = Js1 (Prim "\"nothing\"")
  just x      = Js1 (prim (fun ["x"] "{ just : x }") x)

instance Either (Js2 JsEither) (Js a) (Js b) (Js r) where
  either f g e = Case [("x.left !== undefined", f (Prim "x.left")), ("true", g (Prim "x.right"))] (unJs2 e)
  left l       = Js2 (prim (fun ["x"] "{left  : x}") l)
  right r      = Js2 (prim (fun ["x"] "{right : x}") r)

instance Tuple2 (Js2 JsTuple2) (Js a) (Js b) (Js r) where
  tuple2 f p  = Case [("true", f (Prim "x.fst") (Prim "x.snd"))] (unJs2 p)
  ctuple2 x y = Js2 (prim2 (fun ["x", "y"] "{ fst : x, snd : y }") x y)

instance ListC (Js1 JsList') (Js a) where
  nil         = Js1 (Prim "\"nil\"")
  cons x xs   = Js1 (prim2 (fun ["x", "xs"] "{ head : x, tail : xs }") x (unJs1 xs))

instance ListD (Js1 JsList') (Js a) (Js r) where
  listD x f ys = Case [("x.head === undefined", x), ("true", f (Prim "x.head") (Js1 (Prim "x.tail")))] (unJs1 ys)

-- * JavaScript instances of AwesomePrelude 'type classes'

instance Eq (Js JsBool) (Js JsBool) where
  (==) = BinOp "==="
  (/=) = BinOp "!=="

instance Eq (Js JsNumber) (Js JsBool) where
  (==) = BinOp "==="
  (/=) = BinOp "!=="

