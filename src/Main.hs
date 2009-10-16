module Main where

import Compiler.Compiler
import Generic.Prelude
import Lang.JavaScript
import qualified Prelude as P

mylist :: (NumC j, ListC j) => j [Num]
mylist = 1 `cons` (2 `cons` (3 `cons` (4 `cons` nil)))

sumList :: (NumC j, ListC j, FunC j) => j Num
sumList = sum mylist

jsList :: Js [Num]
jsList = mylist

jsApp :: Js Num
jsApp = id 4

test :: P.IO ()
test = compiler jsApp P.>>= P.putStrLn

