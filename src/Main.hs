{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}

module Main where

import AwesomePrelude
import HaskellPrelude
import JsPrelude

import Prelude (Int, fromInteger)
import qualified Prelude as P

testy :: [Int]
testy = 3 `cons` (4 `cons` nil)

-- test :: P.Bool
test :: Js JsBool
test = not false && false || true

bool' :: a -> a -> P.Bool -> a
bool' x y b = if b then x else y

f :: (forall a. a -> a -> b -> a) -> a -> b -> a
f g a b = g a a b

ok = f bool'
-- epicfail = f bool

hi = maybe false not
       (just true :: P.Maybe P.Bool)
       -- (just true :: JsC1 JsMaybe (Js JsBool))

yo = either (P.* 5) (P.const 2)
       (right false :: P.Either P.Int P.Bool)
       -- (right false :: JsC2 JsEither (Js JsNumber) (Js JsBool))

ah = tuple2 (\x y -> x && P.const false y)
       (ctuple2 true 3 :: (,) P.Bool P.Int)
       -- (ctuple2 true 3 :: JsC2 JsTuple2 (Js JsBool) (Js JsNumber))

