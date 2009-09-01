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


-- Switch between these two types and run `test` to see the AwesomePrelude in action!

fb :: Js JsBool -> Js JsBool
--fb :: P.Bool -> P.Bool
fb x = not x && false || true

test = fb false



--fEq :: Js JsBool -> Js JsBool
fEq :: P.Bool -> P.Bool
fEq x = x == not x

test2 = fEq true



--fNumEq :: Js JsNumber -> Js JsNumber -> Js JsBool
fNumEq :: P.Int -> P.Int -> P.Bool
fNumEq x y = x == y

test3 = fNumEq 3 4



fM :: JsC1 JsMaybe (Js JsBool) -> Js JsBool
--fM :: P.Maybe P.Bool -> P.Bool
fM = maybe false not

test4 = fM (just true)



listy :: [P.Int]
listy = 3 `cons` (4 `cons` nil)

bool' :: a -> a -> P.Bool -> a
bool' x y b = if b then x else y

f :: (forall a. a -> a -> b -> a) -> a -> b -> a
f g a b = g a a b

ok = f bool'
-- epicfail = f bool

-- 
-- yo = either (P.* 5) (P.const 2)
--        (right false :: P.Either P.Int P.Bool)
--        -- (right false :: JsC2 JsEither (Js JsNumber) (Js JsBool))
-- 
-- ah = tuple2 (\x y -> x && P.const false y)
--        (ctuple2 true 3 :: (,) P.Bool P.Int)
--        -- (ctuple2 true 3 :: JsC2 JsTuple2 (Js JsBool) (Js JsNumber))

