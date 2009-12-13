module Compiler.Compiler where

import Control.Applicative
import Compiler.Raw
import Data.List (intercalate)
import Data.Reify
import qualified Lang.Value as Ix

compiler :: Show (Ix.Primitive l) => Ix.Val l i -> IO [Char]
compiler vs = intercalate "\n" . workerJs <$> fromValues vs

workerJs :: Graph Val -> [String]
workerJs = foldVal
  (\_ r0 r1 i f a -> concat r0 ++ "\n" ++ concat r1 ++ "\n"
                  ++ mkAssign i ++ mkId f ++ "(" ++ mkId a ++ ")")
  (\_       i s   -> mkAssign i ++ s)
  (\_ r0    i v b -> mkAssign i ++ "function (" ++ v ++ ")\n{\n" ++ indent (concat r0) ++ "  return " ++ mkId b ++ "\n}")
  (\_       i v   -> mkAssign i ++  v)

indent :: String -> String
indent = unlines . map ("  "++) . lines

mkId :: Int -> String
mkId i = '_':show i

mkAssign :: Int -> String
mkAssign i = ('_':show i) ++ " = "

