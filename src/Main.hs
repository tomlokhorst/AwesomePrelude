module Main where

import Compiler.Pipeline
import Generic.Prelude
import Lang.JavaScript
import qualified Prelude as P

jsList :: Js [Num]
jsList = replicate 700 1

jsSumList :: Js Num
jsSumList = sum (jsList ++ jsList) * sum jsList

jsApp :: Js Num
jsApp = maybe 10 (*2) (just (4 * 3))

main :: P.IO ()
main =
  do out <- compiler jsSumList
     P.writeFile "test.js" out

