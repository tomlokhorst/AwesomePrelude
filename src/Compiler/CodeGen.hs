module Compiler.CodeGen (generateCodeDefinitions, concatDefinitions) where

import Compiler.Raw
import Control.Arrow
import Data.List

concatDefinitions :: Kleisli IO [String] String
concatDefinitions = arr unlines

generateCodeDefinitions :: Kleisli IO Graph [String]
generateCodeDefinitions = arr $
  foldGraph
    (\_ r0 r1 i f a -> concat r0 ++ "\n" ++ concat r1 ++ "\n" ++ assignment i ++ ident f ++ "(" ++ ident a ++ ")")
    (\_       i s   -> assignment i ++ s)
    (\_ r0    i v b -> assignment i ++ "function (" ++ intercalate "," (map ident v) ++ ")\n{\n" ++ indent (concat r0) ++ "  return " ++ ident b ++ "\n}")
    (\_       i v   -> assignment i ++ ident v)
    (\_ r0    i n _ -> assignment n ++ assignment i ++ concat r0)
    (\_ rs    _ _   -> concatMap unlines rs)

indent :: String -> String
indent = unlines . map ("  " ++) . lines

ident :: String -> String
ident i = '_':i

assignment :: String -> String
assignment i = ident i ++ " = "

