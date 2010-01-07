{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Generic.Control.MonadPlus where

import Prelude ()
import Generic.Control.Alternative
import Generic.Control.Monad

class (Alternative j m, Monad j m) => MonadPlus j m

instance (Alternative j m, Monad j m) => MonadPlus j m

