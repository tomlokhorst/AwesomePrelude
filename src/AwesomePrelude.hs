{-# LANGUAGE NoImplicitPrelude, GADTs, MultiParamTypeClasses
           , FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module AwesomePrelude where

class Bool f r | f -> r where
  bool  :: r -> r -> f -> r
  false :: f
  true  :: f

if' :: (Bool b r) => b -> r -> r -> r
if' p t f = bool f t p

(&&) :: (Bool b b) => b -> b -> b
(&&) x y = if' x y x

(||) :: (Bool b b) => b -> b -> b
(||) x y = if' x x y

not :: (Bool b b) => b -> b
not b = if' b false true

otherwise :: (Bool b b) => b
otherwise = true

class Maybe f a r | f -> a, f -> r where
  maybe  :: r -> (a -> r) -> f a -> r
  nothing :: f a
  just    :: a -> f a

class Either f a b r | f -> a, f -> b, f -> r where
  either :: (a -> r) -> (b -> r) -> f a b -> r
  left   :: a -> f a b
  right  :: b -> f a b

class Tuple2 f a b r | f -> a, f -> b, f -> r where
  tuple2  :: (a -> b -> r) -> f a b -> r
  ctuple2 :: a -> b -> f a b

fst :: (Tuple2 f a b a) => f a b -> a
fst = tuple2 (\x _ -> x)

snd :: (Tuple2 f a b b) => f a b -> b
snd = tuple2 (\_ y -> y)

curry :: (Tuple2 f a b r) => (f a b -> c) -> a -> b -> c
curry f x y = f (ctuple2 x y)

uncurry :: (Tuple2 f a b r) => (a -> b -> r) -> f a b -> r
uncurry f t = tuple2 f t

class Eq a where
  (==), (/=) :: Bool b b => a -> a -> b
  
  x /= y = not (x == y)
  x == y = not (x /= y)

class (Eq f) => Ordering f r | f -> r where
  ordering :: r -> r -> r -> f -> r
  lt       :: f
  eq       :: f
  gt       :: f

isLt :: (Ordering o b, Bool b r) => o -> b
isLt = ordering true  false false

isEq :: (Ordering o b, Bool b r) => o -> b
isEq = ordering false true  false

isGt :: (Ordering o b, Bool b r) => o -> b
isGt = ordering false false true


class (Eq a) => Ord a where
  compare              :: (Bool o o, Ordering o r) => a -> a -> o
  (<), (<=), (>), (>=) :: Ordering o r => a -> a -> o

  max, min             :: a -> a -> a

--  compare x y = bool eq lt (x == x) --if_ (x == y)
--                    eq lt
--                    (if_ (x <= y)
--                         lt
--                         gt)

--  x <  y = ordering true  false false (compare x y)
--  x <= y = ordering true  true  false (compare x y)
--  x >  y = ordering false false true  (compare x y)
--  x >= y = ordering false true  true  (compare x y)
--
--  max x y = if_ (x <= y) y x
--  min x y = if_ (x <= y) x y

class List f a r | f -> a, f -> r where
  list :: r -> (a -> r -> r) -> f a -> r
  nil  :: f a
  cons :: a -> f a -> f a

map :: (List f a (g b), List g b r) => (a -> b) -> f a -> g b
map f = list nil (\x ys -> f x `cons` ys)

(++) :: (List f a (g a), List g a r) => f a -> g a -> g a
xs ++ ys = list ys cons xs

filter :: (List f a (g a), Bool b (g a), List g a r) =>
            (a -> b) -> f a -> g a
filter p = list nil (\x xs -> bool xs (x `cons` xs) (p x))

