{-# LANGUAGE
    GADTs
  , KindSignatures
  , EmptyDataDecls
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , FunctionalDependencies
  , TypeOperators
  , RankNTypes
 #-}
module Core.Dot where

import Control.Monad.Identity
import Control.Monad.State
import qualified Core.Val as Ix
import Core.Raw
import Text.Dot
import Data.Reify

frpValues :: Ix.FRP () -> IO (Graph Val)
frpValues = fromIxValues . runIdentity . flip execStateT []

render :: Ix.FRP () -> IO String
render = fmap (showDot . sequence_ . worker) . frpValues

-- Recursive renderer.

worker :: Graph Val -> [Dot ()]
worker = foldVal
  renderApp
  renderArr
  renderComb
  renderComp
  renderPrim

-- Individual renderers.

renderApp :: Nodes -> Int -> Int -> [Int] -> Dot ()
renderApp m i f as =
  do let Prim Ix.Fun d _ = f `from` m
     unode i (filled d "gray")
     mapM_ (\a -> edge (userNodeId a) (userNodeId i) []) as

renderArr :: Nodes -> Int -> Int -> Int -> Dot ()
renderArr _ _ a b = uedge b a (pen "blue")

renderComb :: Nodes -> Int -> [Int] -> Dot ()
renderComb _ i _ = unode i (filled "Comb" "orange")

renderComp :: Nodes -> Int -> Int -> Int -> Dot ()
renderComp _ i _ _ = unode i (filled "Comp" "blue")

renderPrim :: Nodes -> Int -> Ix.Type -> String -> String -> Dot ()
renderPrim _ _ Ix.Fun _ _ = return ()
renderPrim _ i t      d _ = unode i (filled d (typeToColor t))

-- Convert primitive types to colors.

typeToColor :: Ix.Type -> String
typeToColor Ix.Fun   = "gray"
typeToColor Ix.Con   = "green"
typeToColor Ix.Out   = "blue"
typeToColor Ix.In    = "red"
typeToColor Ix.InOut = "orange"
typeToColor Ix.Cast  = "purple"

-- Render helpers.

pen :: String -> [(String, String)]
pen c = [("penwidth", "4"), ("color", c)]

filled :: String -> String -> [(String, String)]
filled l c = [("label", l), ("style", "filled"), ("color", c)]

unode :: Int -> [(String, String)] -> Dot ()
unode i = userNode (userNodeId i)

uedge :: Int -> Int -> [(String, String)] -> Dot ()
uedge i j = edge (userNodeId i) (userNodeId j)

