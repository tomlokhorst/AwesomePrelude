module Main where

import Compiler.Pipeline
import Generic.Prelude
import Lang.JavaScript
import qualified Prelude as P

jsSumList :: Js Num
jsSumList = sum (replicate 3 8 ++ replicate 3 8) * maybe 4 (*8) (just (3 - 2))

main :: P.IO ()
main =
  do out <- compiler jsSumList
     P.writeFile "test.js" out

