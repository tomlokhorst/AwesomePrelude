module Generic.Data.Number where

import Prelude ()
import qualified Prelude

data Num
class NumC j where
  (+) :: j Num -> j Num -> j Num
  (-) :: j Num -> j Num -> j Num
  (*) :: j Num -> j Num -> j Num
  (/) :: j Num -> j Num -> j Num
  num :: Prelude.Integer -> j Num

-- Terrible hack to get number literals working.

instance Prelude.Show (j Num) where
instance Prelude.Eq   (j Num) where
instance NumC j => Prelude.Num (j Num) where
  (+)    = Prelude.undefined
  (*)    = Prelude.undefined
  abs    = Prelude.undefined
  signum = Prelude.undefined
  fromInteger = num

