{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}

module Main where

import AwesomePrelude
import HaskellPrelude
import JsPrelude

import Prelude (Int)
import qualified Prelude as P


-- Switch between these two types to see the AwesomePrelude in action!

f :: Js JsBool -> Js JsBool
--f :: P.Bool -> P.Bool
f x = not x && false || true

test :: Js JsBool
test = true || false && f false

-- kk :: Js JsBool
-- kk = (not (true :: Js JsBool) :: Js JsBool)














-- listy :: [Int]
-- listy = 3 `cons` (4 `cons` nil)

-- bool' :: a -> a -> P.Bool -> a
-- bool' x y b = if b then x else y
-- 
-- f :: (forall a. a -> a -> b -> a) -> a -> b -> a
-- f g a b = g a a b
-- 
-- ok = f bool'
-- -- epicfail = f bool
-- 
-- hi = maybe false not
--        (just true :: P.Maybe P.Bool)
--        -- (just true :: JsC1 JsMaybe (Js JsBool))
-- 
-- yo = either (P.* 5) (P.const 2)
--        (right false :: P.Either P.Int P.Bool)
--        -- (right false :: JsC2 JsEither (Js JsNumber) (Js JsBool))
-- 
-- ah = tuple2 (\x y -> x && P.const false y)
--        (ctuple2 true 3 :: (,) P.Bool P.Int)
--        -- (ctuple2 true 3 :: JsC2 JsTuple2 (Js JsBool) (Js JsNumber))

