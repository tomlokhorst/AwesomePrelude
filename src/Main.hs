module Main where

import Generic.Data.List
import Generic.Data.Number
import Generic.Control.Function
import Lang.JavaScript
import Compiler.Compiler
import qualified Prelude as P

mylist :: (NumC f, ListC f) => f [Num]
mylist = 1 `cons` (2 `cons` (3 `cons` (4 `cons` nil)))

sumList :: (NumC f, ListC f, FunC f) => f Num
sumList = sum mylist

jsList :: Js Num
jsList = sumList

test :: P.IO ()
test = compiler jsList P.>>= P.putStrLn

