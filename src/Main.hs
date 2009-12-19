module Main where

import Compiler.Pipeline
import Generic.Prelude
import Lang.JavaScript
import qualified Prelude as P

jsList :: Js [Num]
jsList = 1 `cons` (2 `cons` (3 `cons` (4 `cons` (5 `cons` (6 `cons` (7 `cons` nil))))))

jsSumList :: Js Num
jsSumList = sum jsList

jsApp :: Js Num
jsApp = maybe 10 (*2) (just (4 * 3))

main :: P.IO ()
main =
  do out <- compiler jsSumList
     P.writeFile "test.js" out

