{-# LANGUAGE EmptyDataDecls #-}

module Generic.Data.Bool where

import Prelude ()

data Bool
class BoolC j where
  false :: j Bool
  true  :: j Bool
  bool  :: j a -> j a -> j Bool -> j a

if' :: BoolC j => j Bool -> j a -> j a -> j a
if' b x y = bool y x b

(&&) :: BoolC j => j Bool -> j Bool -> j Bool
x && y = bool false y x

(||) :: BoolC j => j Bool -> j Bool -> j Bool
x || y = bool y true x

not :: BoolC j => j Bool -> j Bool
not = bool true false

