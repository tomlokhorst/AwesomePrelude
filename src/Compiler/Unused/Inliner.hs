module Compiler.Inliner where

import qualified Data.Map as Map
import Data.List
import Compiler.Raw
import Control.Arrow

inliner (Graph vm x) = 
  let uses = map snd . filter ((<=1) . fst) . map (length &&& head) . group . sort . concatMap undefined . Map.elems $ vm :: [String]
      rep from to x = if x == from then to else x
      once (f, t) = Map.map (fmap (rep f t))
  in Graph (foldr once vm uses) x

