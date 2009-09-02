{-# LANGUAGE
    GADTs
  , EmptyDataDecls
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
 #-}

module JsPrelude where

import AwesomePrelude
import qualified Prelude as P
import Data.List

-- * JavaScript DSL

newtype JsC1 f a   = JsC1 { unJsC1 :: Js (f a) }
newtype JsC2 f a b = JsC2 { unJsC2 :: Js (f a b) }

data Js a where
  Prim     :: P.String -> Js a
  App      :: Js (a -> b) -> Js a -> Js b
  Case     :: [(P.String, Js a)] -> Js b -> Js c
  BinOp    :: P.String -> Js a -> Js b -> Js c
  TriOp    :: P.String -> P.String -> Js a -> Js b -> Js c -> Js d

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
showJs (BinOp s   a b)   = "(" P.++ showJs a P.++ s P.++ showJs b P.++ ")"
showJs (TriOp s t a b c) = "(" P.++ showJs a P.++ s P.++ showJs b P.++ t P.++ showJs c P.++ ")"
showJs p@(App _ _)       = fu p P.++ "(" P.++ intercalate "," (args p) P.++ ")"
  where
    fu :: Js a -> P.String
    fu (App f _) = fu f
    fu x         = showJs x

    args :: Js a -> [P.String]
    args (App f' a) = args f' P.++ [showJs a]
    args _          = []

instance P.Show (f a) => P.Show (JsC1 f a) where
  show jc = P.show (unJsC1 jc)

instance P.Show a => P.Show (JsMaybe a) where
  show ls = P.show ls

instance P.Show a => P.Show (JsList a) where
  show ls = P.show ls

data JsBool
data JsNumber
data JsMaybe a
data JsEither a b
data JsTuple2 a b
data JsList a

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

instance Bool (Js JsBool) (Js r) where
  bool f t p = TriOp "?" ":" p t f
  true       = Prim "true"
  false      = Prim "false"

instance Maybe (JsC1 JsMaybe) (Js a) (Js r) where
  maybe x f m = Case [("x.just !== undefined", f (Prim "x.just")), ("true", x)] (unJsC1 m)
  nothing     = JsC1 (Prim "\"nothing\"")
  just x      = JsC1 (prim (fun ["x"] "{ just : x }") x)

instance Either (JsC2 JsEither) (Js a) (Js b) (Js r) where
  either f g e = Case [("x.left !== undefined", f (Prim "x.left")), ("true", g (Prim "x.right"))] (unJsC2 e)
  left l       = JsC2 (prim (fun ["x"] "{left  : x}") l)
  right r      = JsC2 (prim (fun ["x"] "{right : x}") r)

instance Tuple2 (JsC2 JsTuple2) (Js a) (Js b) (Js r) where
  tuple2 f p  = Case [("true", f (Prim "x.fst") (Prim "x.snd"))] (unJsC2 p)
  ctuple2 x y = JsC2 (prim2 (fun ["x", "y"] "{ fst : x, snd : y }") x y)

instance List (JsC1 JsList) (Js a) (Js r) where
  list x f ys = Case [("x.head === undefined", x), ("true", f (Prim "x.head") (Prim "rec(x.tail)"))] (unJsC1 ys)
  nil         = JsC1 (Prim "\"nil\"")
  cons x xs   = JsC1 (prim2 (fun ["x", "xs"] "{ head : x, tail : xs }") x (unJsC1 xs))

-- * JavaScript instances of AwesomePrelude 'type classes'

instance Eq (Js JsBool) (Js JsBool) where
  (==) = BinOp "==="
  (/=) = BinOp "!=="

instance Eq (Js JsNumber) (Js JsBool) where
  (==) = BinOp "==="
  (/=) = BinOp "!=="

