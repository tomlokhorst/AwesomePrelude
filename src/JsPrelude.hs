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
--import Prelude hiding (Maybe, Either, Bool, Eq, (==), (&&), (++))
import Data.List

newtype JsC1 f a   = JsC1 { unJsC1 :: Js (f a) }
newtype JsC2 f a b = JsC2 { unJsC2 :: Js (f a b) }

data Js a where
  Prim     :: P.String -> Js a
  App      :: Js (a -> b) -> Js a -> Js b
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
showJs (Prim s)       = s
showJs (Destruct s x) = P.show (App (Prim s) x)
showJs p@(App f x)    = fun p P.++ "(" P.++ intercalate "," (args p) P.++ ")"
  where
    fun :: Js a -> P.String
    fun (App f _) = fun f
    fun x         = showJs x

    args :: Js a -> [P.String]
    args (App f a) = args f P.++ [showJs a]
    args _         = []
 

data JsBool

instance Bool (Js JsBool) (Js r) where
  bool f t x = prim3 "(function (f, t, x) x ? t : f)" f t x
  true       = Prim "true"
  false      = Prim "false"

data JsMaybe a

instance Maybe (JsC1 JsMaybe) (Js a) (Js r) where
  maybe  x f m =
    let m'      = unJsC1 m
        patJust = Destruct "(function (x) x.just)" m'
    in prim3 "(function(x, y, m) m.just === undefined ? x : y)" x (f patJust) m'
  nothing = JsC1 (Prim "{ }")
  just x  = JsC1 (prim "(function (x) { return { just : x }})" x)


data JsTuple2 a b

instance Tuple2 (JsC2 JsTuple2) (Js a) (Js b) (Js r) where
  tuple2 f p =
    let p' = unJsC2 p
        patFst = Destruct ("(function (p) p.fst)") p'
        patSnd = Destruct ("(function (p) p.snd)") p'
    in prim "(function (z, p) z)" (f patFst patSnd)
  ctuple2 x y = JsC2 (prim2 "(function (x, y) { return { fst : x, snd : y }})" x y)

data JsEither a b

instance Either (JsC2 JsEither) (Js a) (Js b) (Js r) where
  either f g e =
    let e'       = unJsC2 e
        patLeft  = Destruct "(function (x) x.left)" e'
        patRight = Destruct "(function (x) x.left)" e'
    in prim3 "(function (x, y, e) e.right === undefined ? x : y)" (f patLeft) (g patRight) e'
  left l  = JsC2 (prim "(function (x) { return { left : x }})" l)
  right r = JsC2 (prim "(function (x) { return {right : x }})" r)

data JsNumber

jsEq :: Js a -> Js a -> Js JsBool
jsEq a b = Prim "function (a, b) { return a === b; }" `App` a `App` b

jsNeq :: Js a -> Js a -> Js JsBool
jsNeq a b = Prim "function (a, b) { return a !== b; }" `App` a `App` b

instance Eq (Js JsBool) (Js JsBool) where
  (==) = jsEq
  (/=) = jsNeq

instance Eq (Js JsNumber) (Js JsBool) where
  (==) = jsEq
  (/=) = jsNeq

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


