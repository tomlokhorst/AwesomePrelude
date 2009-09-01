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
  PreOp    :: P.String -> Js a -> Js b
  BinOp    :: P.String -> Js a -> Js b -> Js c
  TriOp    :: P.String -> P.String -> Js a -> Js b -> Js c -> Js d
  Destruct :: P.String -> Js f -> Js a

prim :: P.String -> Js a -> Js b
prim f a = Prim f `App` a

prim2 :: P.String -> Js a -> Js b -> Js c 
prim2 f a b = Prim f `App` a `App` b

prim3 :: P.String -> Js a -> Js b -> Js c -> Js d 
prim3 f a b c = Prim f `App` a `App` b `App` c

instance P.Show (Js a) where
  show = showJs

showJs :: Js a -> P.String
showJs (PreOp s   a)     = "(" P.++ s P.++ showJs a P.++ ")"
showJs (BinOp s   a b)   = "(" P.++ showJs a P.++ s P.++ showJs b P.++ ")"
showJs (TriOp s t a b c) = "(" P.++ showJs a P.++ s P.++ showJs b P.++ t P.++ showJs c P.++ ")"
showJs (Prim s)          = s
showJs (Destruct s x)    = P.show (App (Prim s) x)
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
  abs           = prim (fun ["x"] "Math.abs(x)")
  signum        = PreOp "-"
  fromInteger x = Prim (P.show x)

instance P.Eq (Js JsNumber) where
  (==) = P.error "Prelude.Eq shouldn't be used with the AwesomePrelude"
  (/=) = P.error "Prelude.Eq shouldn't be used with the AwesomePrelude"


-- * JavaScript instances for AwesomePrelude 'data types'

instance Bool (Js JsBool) (Js r) where
  bool  = \f t p -> TriOp "?" ":" p t f
  true  = Prim "true"
  false = Prim "false"

instance Maybe (JsC1 JsMaybe) (Js a) (Js r) where
  maybe  x f m =
    let m'      = unJsC1 m
        patJust = Destruct (fun ["x"] "x.just") m'
    in prim3 (fun ["x", "y", "m"] "m.just === undefined ? x : y") x (f patJust) m'
  nothing = JsC1 (Prim "\"nothing\"")
  just x  = JsC1 (prim (fun ["x"] "just : x") x)

instance Either (JsC2 JsEither) (Js a) (Js b) (Js r) where
  either f g e =
    let e'       = unJsC2 e
        patLeft  = Destruct (fun ["x"] "x.left")  e'
        patRight = Destruct (fun ["x"] "x.right") e'
    in prim3 (fun ["x", "y", "e"] "e.right === undefined ? x : y") (f patLeft) (g patRight) e'
  left l  = JsC2 (prim (fun ["x"] "{left  : x}") l)
  right r = JsC2 (prim (fun ["x"] "{right : x}") r)

instance Tuple2 (JsC2 JsTuple2) (Js a) (Js b) (Js r) where
  tuple2 f p =
    let p' = unJsC2 p
        patFst = Destruct (fun ["p"] "p.fst") p'
        patSnd = Destruct (fun ["p"] "p.snd") p'
    in prim2 (fun ["z", "p"] "z") (f patFst patSnd) p'
  -- ctuple2 :: a -> b -> f a b
  ctuple2 x y = JsC2 (prim2 (fun ["x", "y"] "{ fst : x, snd : y }") x y)

instance List (JsC1 JsList) (Js a) (Js r) where
  -- list :: r -> (a -> r -> r) -> f a -> r
  list x f ys =
    let ys'     = unJsC1 ys
        patHead = Destruct (fun ["l"] "x.head") ys'
        patTail = Destruct (fun ["l"] "x.tail") ys'
    in prim3 (fun ["x", "y", "zs"] "zs.head === undefined ? x : y") x (f patHead patTail) ys'
  nil = JsC1 (Prim "\"nil\"")
  -- cons :: a -> f a -> f a
  cons x xs =
    let xs'  = unJsC1 xs
    in JsC1 (prim2 (fun ["x", "xs"] "{ head : x, tail : xs }") x xs')

-- * JavaScript instances of AwesomePrelude 'type classes'

instance Eq (Js JsBool) (Js JsBool) where
  (==) = BinOp "==="
  (/=) = BinOp "!=="

instance Eq (Js JsNumber) (Js JsBool) where
  (==) = BinOp "==="
  (/=) = BinOp "!=="

-- instance Num (Js JsNumber) where
--   (+) = prim2 "(function(a,b){return a+b})"
--   (*) = prim2 "(function(a,b){return a*b})"
--   (-) = prim2 "(function(a,b){return a-b})"
--   abs = prim "(Math.abs)"
--   signum = prim "(Math.sign)"
--   fromInteger = Prim . show
-- 
-- instance Fractional (Js JsNumber) where
--   (/) = prim2 "$(function(a,b){return a/b})"
--   fromRational = Prim . show


