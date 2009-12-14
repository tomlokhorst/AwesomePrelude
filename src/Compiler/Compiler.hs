module Compiler.Compiler where

import Control.Monad
import Control.Applicative
import Compiler.LambdaLifting
import Compiler.Raw
import Data.List (intercalate)
import Data.Reify
import qualified Lang.Value as Ix
import qualified Data.Reify.Graph.CSE as CSE
-- fromValues :: Show (Ix.Primitive l) => Ix.Val l i -> IO (Graph Val)

-- Dealing with multiple values.

compiler :: Show (Ix.Primitive l) => Ix.Val l i -> IO String
compiler inp = 
  do x <- reifyGraph . (In . More) . lambdaLift . raw $ inp
     return (unlines . workerJs . renamer {-. CSE.cse-} $ x)


workerJs :: (ValMap, String) -> [String]
workerJs = foldVal
  (\_ r0 r1 i f a -> concat r0 ++ "\n" ++ concat r1 ++ "\n"
                  ++ mkAssign i ++ mkId f ++ "(" ++ mkId a ++ ")")
  (\_       i s   -> mkAssign i ++ s)
  (\_ r0    i v b -> mkAssign i ++ "function (" ++ intercalate "," (map ('_':) v) ++ ")\n{\n" ++ indent (concat r0) ++ "  return " ++ mkId b ++ "\n}")
  (\_       i v   -> mkAssign i ++ '_':v)
  (\_ r0    i n _ -> mkAssign n ++ mkAssign i ++ concat r0)
  (\_ rs    i as  -> concatMap unlines rs)

indent :: String -> String
indent = unlines . map ("  "++) . lines

mkId :: String -> String
mkId i = '_':i

mkAssign :: String -> String
mkAssign s = '_':s ++ " = "

