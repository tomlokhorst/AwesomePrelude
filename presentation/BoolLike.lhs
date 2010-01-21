%include polycode.fmt

%if style == newcode

> {-# OPTIONS_GHC -F -pgmF lhs2tex -optF --pre #-}

> module BoolLike where
> import Expr 

%endif


\begin{frame}

%if style /= newcode

> (+) :: Num a => a -> a -> a

> (==) :: Eq a => a -> a -> Bool

\pause

> (==) :: (Eq a, BoolLike b) => a -> a -> b

%endif

\end{frame}


\begin{frame}

> class BoolLike b where
>   false  :: b
>   true   :: b
>   bool   :: a -> a -> b -> a

\begin{overlayarea}{\textwidth}{0.2\textheight}
\only<2>{

> instance BoolLike Bool where
>   false       = False
>   true        = True
>   bool x y b  = if b then y else x

}
\only<3>{

> instance BoolLike (Expr Bool) where
>   false       = ConFalse
>   true        = ConTrue
>   bool x y b  = If b y x

}
\end{overlayarea}
 
\end{frame}


