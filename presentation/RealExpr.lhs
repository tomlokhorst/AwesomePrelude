%include polycode.fmt
%include code.fmt

%if style == newcode

> {-# OPTIONS_GHC -F -pgmF lhs2tex -optF --pre #-}

> module Expr where

%endif


\begin{frame}

%if style /= newcode

> if 2 + 3 == 5
> then  1
> else  0

%endif

\pause

> data Expr a where
>   Con       ::  Int        ->  Expr Int
>   Add       ::  Expr Int   ->  Expr Int  ->  Expr Int
>   Mul       ::  Expr Int   ->  Expr Int  ->  Expr Int
>   ConFalse  ::  Expr Bool
>   ConTrue   ::  Expr Bool
>   Eq        ::  Expr Int   ->  Expr Int  ->  Expr Bool
>   If        ::  Expr Bool  ->  Expr a    ->  Expr a        ->  Expr a
  
\end{frame}


\begin{frame}

> eval :: Expr a -> a
> eval (Con x)     =  x
> eval (Add x y)   =  eval x + eval y
> eval (Mul x y)   =  eval x * eval y
> eval (ConFalse)  =  False
> eval (ConTrue)   =  True
> eval (Eq x y)    =  eval x == eval y
> eval (If p x y)  =  if eval p
>                     then eval x
>                     else eval y

\end{frame}


\begin{frame}

%if style /= newcode

> if 2 + 3 == 5
> then  1
> else  0

\pause

\begin{overlayarea}{\textwidth}{0.2\textheight}
\only<2>{

> x :: Expr Int
> x = If  (Eq  (Add (Con 2) (Con 3))
>              (Con 5))
>         (Con 1)
>         (Con 0)

}
\only<3>{

> x :: Expr Int
> x = If  (Eq (2 + 3) 5) (1) (0)

}
%format ==        = "=="
\only<4>{

> x :: Expr Int
> x = If  (2 + 3 == 5) (1) (0)

}
\end{overlayarea}

%endif

%if style == newcode

> instance Show (Expr a) where
>   show (Con x)     = "Con " ++ show x
>   show (Add x y)   = "Add (" ++ show x ++ ") (" ++ show y ++ ")"
>   show (Mul x y)   = "Mul (" ++ show x ++ ") (" ++ show y ++ ")"
>   show (ConFalse)  = "ConFalse"
>   show (ConTrue)   = "ConTrue"
>   show (Eq x y)    = "Eq (" ++ show x ++ ") (" ++ show y ++ ")"
>   show (If p x y)  = "If (" ++ show p ++ ") (" ++ show x ++ ") (" ++ show y ++ ")"

> instance Eq (Expr a) where
>   Con x   == Con y     = x == y
>   Add x y == Add x' y' = x == x' && y == y'
>   Mul x y == Mul x' y' = x == x' && y == y'
>   _       == _         = False

> instance Num (Expr Int) where
>   fromInteger  =  Con . fromIntegral
>   x  +  y      =  Add x y
>   x  *  y      =  Mul x y
>   abs          =  undefined
>   signum       =  undefined

%endif

\end{frame}

