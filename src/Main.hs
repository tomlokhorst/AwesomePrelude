module Main where

import Compiler.Pipeline
import Generic.Prelude
import Lang.JavaScript
import qualified Prelude as P

three :: JavaScript Number
three = 3

jsSumList :: JavaScript Number
jsSumList = sum (replicate three (2 * 8) ++ replicate three 8) * maybe 4 (*8) (just (3 - 2))

main :: P.IO ()
main = do
  out <- compiler jsSumList
  P.writeFile "test.js" out

