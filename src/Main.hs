module Main where

import Compiler.Compiler
import Compiler.Raw
import Generic.Prelude
import Lang.JavaScript
import qualified Prelude as P

mylist :: (NumC j, ListC j, FunC j) => j [Num]
mylist = 1 `cons` (2 `cons` (3 `cons` (4 `cons` nil)))

sumList :: (NumC j, ListC j, FunC j) => j Num
sumList = sum mylist

jsList :: Js [Num]
jsList = mylist

jsSumList :: Js Num
jsSumList = sum jsList

jsApp :: Js Num
jsApp = maybe 10 (*2) (just (4 * 3))

test :: P.IO ()
test = compiler jsApp P.>>= P.putStrLn

-- test2 :: P.IO ()
-- test2 = P.print (raw jsApp)

