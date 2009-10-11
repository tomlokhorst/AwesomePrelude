module Generic.Data.Number where

import Prelude ()
import qualified Prelude

data Num
class NumC f where
  (+) :: f Num -> f Num -> f Num
  (-) :: f Num -> f Num -> f Num
  (*) :: f Num -> f Num -> f Num
  (/) :: f Num -> f Num -> f Num
  num :: Prelude.Integer -> f Num

-- Terrible hack to get number literals working.

instance Prelude.Show (f Num) where
instance Prelude.Eq   (f Num) where
instance NumC f => Prelude.Num (f Num) where
  (+)    = Prelude.undefined
  (*)    = Prelude.undefined
  abs    = Prelude.undefined
  signum = Prelude.undefined
  fromInteger = num

