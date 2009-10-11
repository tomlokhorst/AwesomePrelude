module Core.Compiler where

import Control.Monad.Identity
import Control.Monad.State
import Core.Raw
import Data.List (intercalate)
import Data.Reify
import qualified Core.Val as Ix

frpValues :: Ix.FRP () -> IO (Graph Val)
frpValues = fromIxValues . runIdentity . flip execStateT []

compiler :: Ix.FRP () -> IO String
compiler = fmap (intercalate "\n" . workerJs) . frpValues

workerJs :: Graph Val -> [String]
workerJs = foldVal
  (\_ i f a   -> mkAssign i ++ mkId f ++ "(" ++ intercalate "," (map mkId a) ++ ")")
  (\_ i a b   -> mkAssign i ++ mkId a ++ "(" ++ mkId b ++ ")")
  (\_ i fs    -> mkAssign i ++ "lift(listify)(" ++ intercalate "," (map mkId fs) ++ ")")
  (\_ i a b   -> mkAssign i ++ "C(" ++ mkId a ++ "," ++ mkId b ++ ")")
  (\_ i t _ s -> mkAssign i ++ if t == Ix.Con then "reactive(" ++ s ++ ")" else s)

mkId :: Int -> String
mkId i = '_':show i

mkAssign :: Int -> String
mkAssign i = ('_':show i) ++ " = "

