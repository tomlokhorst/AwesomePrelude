module Compiler.CodeGen (generateCodeDefinitions, concatDefinitions) where

import Compiler.Raw
import Control.Arrow
import Data.List

-- Generic value traversal.

foldGraph
  :: (Graph -> [a] -> [a] -> String -> String -> String                 -> a)
  -> (Graph               -> String -> String                           -> a)
  -> (Graph               -> String -> ([String] -> String) -> [String] -> a)
  -> (Graph -> [a]        -> String -> [String] -> String               -> a)
  -> (Graph               -> String -> String                           -> a)
  -> (Graph -> [a]        -> String -> String -> String                 -> a)
  -> (Graph -> [[a]]      -> String -> [String]                         -> a)
  -> Graph
  -> [a]
foldGraph f0 f1 f2 f3 f4 f5 f6 g@(Graph m r) = evalState (folder (r, r `from` m)) Set.empty
  where
    folder (i, term) =
      case term of
        App f b   -> rec f >>= \r0 -> rec b >>= \r1 -> pure [f0 g r0 r1 i f b  ]
        Con s     ->                                   pure [f1 g       i s    ]
        Prim s vs ->                                   pure [f2 g       i s vs ]
        Lam v b   -> rec b >>= \r0 ->                  pure [f3 g r0    i v b  ]
        Var v     ->                                   pure [f4 g       i v    ]
        More as   ->            mapM rec as >>= \rs -> pure [f6 g rs    i as   ]
        Def n b   -> rec b >>= \r0 ->                  pure [f5 g r0    i n b  ]
    from f = fromMaybe (error "internal error in foldGraph") . lookup f
    rec k =
      do v <- gets (Set.member k)
         modify (Set.insert k)
         if not v then folder (k, k `from` m) else return mempty

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

