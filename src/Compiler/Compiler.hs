module Compiler.Compiler where

import Control.Applicative
import Compiler.Raw
import Data.List (intercalate)
import Data.Reify
import qualified Lang.Value as Ix

compiler :: Ix.Val l i -> IO String
compiler vs = intercalate "\n" . workerJs <$> fromValues vs

workerJs :: Graph Val -> [String]
workerJs = foldVal
  (\_ i f a -> mkAssign i ++ mkId f ++ "(" ++ mkId a ++ ")")
  (\_ i s   -> mkAssign i ++ s)

mkId :: Int -> String
mkId i = '_':show i

mkAssign :: Int -> String
mkAssign i = ('_':show i) ++ " = "

