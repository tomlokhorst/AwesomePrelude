module Generic.Control.Functor where

class Functor j f where
  fmap :: (j a -> j b) -> j (f a) -> j (f b)

