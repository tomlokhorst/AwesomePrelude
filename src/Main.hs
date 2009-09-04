{-# LANGUAGE
    NoImplicitPrelude
  , RankNTypes
 #-}

module Main where

import AwesomePrelude
import HaskellPrelude
import JsPrelude

import Prelude (fromInteger)
import qualified Prelude as P


-- Switch between the two types and run `test*` to see the AwesomePrelude in action!

fb :: Js JsBool -> Js JsBool
--fb :: P.Bool -> P.Bool
fb x = not x && false || true

test = fb false



fEq :: Js JsBool -> Js JsBool
--fEq :: P.Bool -> P.Bool
fEq x = x == fb x

test2 = fEq false



fNumEq :: Js JsNumber -> Js JsNumber -> Js JsBool
--fNumEq :: P.Int -> P.Int -> P.Bool
fNumEq x y = x == (y P.- 1)

test3 = fNumEq 3 4



fM :: JsC1 JsMaybe (Js JsBool) -> Js JsBool
--fM :: P.Maybe P.Bool -> P.Bool
fM = maybe false not

test4 = fM (just false)



listy :: JsC1 JsList (Js JsNumber)
--listy :: [P.Int]
listy = 2 `cons` (3 `cons` (4 `cons` nil))



-- fL :: JsC1 JsList (Js JsNumber) -> Js JsNumber
fL :: [P.Int] -> P.Int
fL = sum
-- 
-- test5 = fL listy



-- Weird test case provided by Erik Hesselink:

bool' :: a -> a -> P.Bool -> a
bool' x y b = if b then x else y

f :: (forall a. a -> a -> b -> a) -> a -> b -> a
f g a b = g a a b

ok = f bool'
--epicfail = f bool

