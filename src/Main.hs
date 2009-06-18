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

if' :: a -> a -> P.Bool -> a
if' x y b = if b then x else y

f :: (forall a. a -> a -> b -> a) -> a -> b -> a
f g a b = g a a b

ok = f if'
-- epicfail = f bool

-- hoi = maybee false (not_) (just true :: JsC1 JsMaybe (Js JsBool))

-- yo = eitherr (*5) (const 2) (right false :: JsC2 JsEither (Js JsNumber) (Js JsBool))

-- even' :: (Js JsNumber) -> (Js JsBool)
-- even' x = false

-- ahh = tuple2 (\x y -> x `and_` even' y) (ctuple2 true 3 :: JsC2 JsTuple2 (Js JsBool) (Js JsNumber))

