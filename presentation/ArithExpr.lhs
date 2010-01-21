%include polycode.fmt

%if style == newcode

> {-# OPTIONS_GHC -F -pgmF lhs2tex -optF --pre #-}

> module ArithExpr where
> import Language.Cil hiding (Add)

%endif

\begin{frame}

> data Expr where
>   Con       ::  Int   ->  Expr
>   Add       ::  Expr  ->  Expr  ->  Expr
>   Mul       ::  Expr  ->  Expr  ->  Expr

%if style == newcode

> instance Show Expr where
>   show (Con x)   = "Con " ++ show x
>   show (Add x y) = "Add (" ++ show x ++ ") (" ++ show y ++ ")"
>   show (Mul x y) = "Mul (" ++ show x ++ ") (" ++ show y ++ ")"

> instance Eq Expr where
>   Con x   == Con y     = x == y
>   Add x y == Add x' y' = x == x' && y == y'
>   Mul x y == Mul x' y' = x == x' && y == y'
>   _       == _         = False

%endif

\end{frame} 


\begin{frame}

> eval :: Expr -> Int
> eval (Con x)    =  x
> eval (Add x y)  =  eval x + eval y
> eval (Mul x y)  =  eval x * eval y

\end{frame} 


\begin{frame}


%if style == newcode

> mul :: MethodDecl
> mul = add -- Yeah, not implemeted

%endif


%if style /= newcode

> import Language.Cil

%endif

> compile :: Expr -> Assembly
> compile e  =  simpleAssembly (f e)
>   where
>     f :: Expr -> [MethodDecl]
>     f (Con x)    =  [ldc_i4 x]
>     f (Add x y)  =  f x ++ f y ++ [add]
>     f (Mul x y)  =  f x ++ f y ++ [mul]

\end{frame} 


\begin{frame}

\begin{center}
\Huge{$4 + 2 \times 3$}
\end{center}

\pause

%if style /= newcode

> x :: Expr
> x =  Add  (Con 4)
>           (Mul  (Con 2)
>                 (Con 3))

%endif

\end{frame} 


\begin{frame}

> instance Num Expr where
>   fromInteger x  =  Con (fromIntegral x)
>   x  +  y        =  Add x y
>   x  *  y        =  Mul x y

%if style == newcode

>   abs            =  undefined
>   signum         =  undefined

%endif

\end{frame} 


\begin{frame}

\begin{center}
\Huge{$4 + 2 \times 3$}
\end{center}

\pause

%if style /= newcode

\begin{overlayarea}{\textwidth}{0.2\textheight}
\only<2>{

> x :: Expr
> x =  4 + 2 * 3

}

\only<3>{

> x :: Int
> x =  4 + 2 * 3

}
\only<4>{

> x :: Num a => a
> x =  4 + 2 * 3

}

\end{overlayarea}

%endif

\end{frame} 

