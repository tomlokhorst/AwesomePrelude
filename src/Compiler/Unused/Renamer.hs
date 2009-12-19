module Compiler.Renamer (renameNamedDefinitions) where

import Compiler.Raw
import Control.Arrow
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Reify as R

renameNamedDefinitions :: Kleisli IO (R.Graph ExprF) Graph
renameNamedDefinitions = arr $ \(R.Graph xs r) -> 
  let strings = map (\(u, tu) -> (show u, fmap show tu)) xs
      named   = catMaybes (map getName strings)
  in Graph (foldr replacer (M.fromList strings) named) (show r)

getName :: (String, ExprF String) -> Maybe (String, Name, String)
getName (from, Name to f) = Just (from, to, f)
getName _                 = Nothing

replacer :: (String, String, String) -> M.Map String (ExprF String) -> M.Map String (ExprF String)
replacer (from, to, f) m =
  let Just x = M.lookup f m
      rep i = if i == from then to else i
  in M.delete from . M.insert to x . M.map (fmap rep) $ m

