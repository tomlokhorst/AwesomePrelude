{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import AwesomePrelude
import HaskellPrelude
import JsPrelude

import Prelude (Int, fromInteger)

testy :: [Int]
testy = (3 :: Int) `cons` (4 `cons` nil)

test :: Js JsBool
test = not false && false || true

-- hoi = maybee false (not_) (just true :: JsC1 JsMaybe (Js JsBool))

-- yo = eitherr (*5) (const 2) (right false :: JsC2 JsEither (Js JsNumber) (Js JsBool))

-- even' :: (Js JsNumber) -> (Js JsBool)
-- even' x = false

-- ahh = tuple2 (\x y -> x `and_` even' y) (ctuple2 true 3 :: JsC2 JsTuple2 (Js JsBool) (Js JsNumber))

