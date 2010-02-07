%include polycode.fmt
%include code.fmt

%if style == newcode

> {-# OPTIONS_GHC -F -pgmF lhs2TeX -optF --pre #-}

> module Expr where

%endif


\begin{frame}

%if style /= newcode

\center{\huge{$\mathrm{if}\; 2 + 3 \equiv 5\;\; \mathrm{then}\; 1\;\; \mathrm{else}\; 0$}}

%endif

\pause

> WhiteSpace
> data Expr where
>   Con       ::  Int   ->  Expr
>   Add       ::  Expr  ->  Expr  ->  Expr 
>   Mul       ::  Expr  ->  Expr  ->  Expr 
>   ConFalse  ::  Expr 
>   ConTrue   ::  Expr 
>   Eq        ::  Expr  ->  Expr  ->  Expr 
>   If        ::  Expr  ->  Expr  ->  Expr  ->  Expr
  
\end{frame}


\begin{frame}

> eval :: Expr -> Either Bool Int
> eval (Con x)     =  Right x
> eval (Add x y)   =  let  (Right x')  = eval x
>                          (Right y')  = eval y
>                     in   Right (x' + y')
> eval (Mul x y)   =  let  (Right x')  = eval x
>                          (Right y')  = eval y
>                     in   Right (x' * y')
> eval (ConFalse)  =  Left False
> eval (ConTrue)   =  Left True
> eval (Eq x y)    =  Left (eval x == eval y)
> eval (If p x y)  =  let  (Left p')  = eval p
>                     in   if p'
>                          then  eval x
>                          else  eval y

\end{frame}


\begin{frame}

%if style /= newcode

\center{\huge{$\mathrm{if}\; 2 + 3 \equiv 5\;\; \mathrm{then}\; 1\;\; \mathrm{else}\; 0$}}

\pause

\begin{overlayarea}{\textwidth}{0.3\textheight}
\only<2>{

> x :: Expr
> x = If  (Eq  (Add (Con 2) (Con 3))
>              (Con 5))
>         (Con 1)
>         (Con 0)

}
\only<3>{

> x :: Expr
> x = If  (Eq (2 + 3) 5) (1) (0)

}
\only<4>{

> x :: Expr
> x = If  (2 + 3 == 5) (1) (0)

}
\end{overlayarea}

%endif

%if style == newcode

> instance Show Expr where
>   show (Con x)     = "Con " ++ show x
>   show (Add x y)   = "Add (" ++ show x ++ ") (" ++ show y ++ ")"
>   show (Mul x y)   = "Mul (" ++ show x ++ ") (" ++ show y ++ ")"
>   show (ConFalse)  = "ConFalse"
>   show (ConTrue)   = "ConTrue"
>   show (Eq x y)    = "Eq (" ++ show x ++ ") (" ++ show y ++ ")"
>   show (If p x y)  = "If (" ++ show p ++ ") (" ++ show x ++ ") (" ++ show y ++ ")"

> instance Eq Expr where
>   Con x   == Con y     = x == y
>   Add x y == Add x' y' = x == x' && y == y'
>   Mul x y == Mul x' y' = x == x' && y == y'
>   _       == _         = False

> instance Num Expr where
>   fromInteger  =  Con . fromIntegral
>   x  +  y      =  Add x y
>   x  *  y      =  Mul x y
>   abs          =  undefined
>   signum       =  undefined

%endif

\end{frame}

