%include polycode.fmt
%include code.fmt

%if style == newcode

> {-# OPTIONS_GHC -F -pgmF lhs2TeX -optF --pre #-}

> module BoolLike where
> import Expr 

%endif


\begin{frame}

%if style /= newcode

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

\pause

%if style /= newcode

\begin{overlayarea}{\textwidth}{0.3\textheight}
\only<2>{

> instance BoolLike Bool where
>   false       = False
>   true        = True
>   bool x y b  = if b then y else x

}
\only<3>{

> instance BoolLike Expr where
>   false       = ConFalse
>   true        = ConTrue
>   bool x y b  = If b y x

}
\end{overlayarea}

%endif

\end{frame}


\begin{frame}

%if style /= newcode

> (&&)  :: Bool -> Bool -> Bool
> (||)  :: Bool -> Bool -> Bool
> not   :: Bool -> Bool


\pause

\begin{overlayarea}{\textwidth}{0.5\textheight}
\only<2>{

> (&&)  :: BoolLike b => b -> b -> b
> WhiteSpace

> (||)  :: BoolLike b => b -> b -> b
> WhiteSpace

> not   :: BoolLike b => b -> b
> WhiteSpace

}
\only<3>{

> (&&)  :: BoolLike b => b -> b -> b
> (&&) x y = bool x y x

> (||)  :: BoolLike b => b -> b -> b
> (||) x y = bool y x x

> not   :: BoolLike b => b -> b
> not x = bool true false x

}
\end{overlayarea}

%endif

\end{frame}


\begin{frame}

%if style /= newcode

\textcolor{cmnt}{
\begin{verbatim}

ghci> :t not
not :: (BoolLike b) => b -> b

ghci> not True
False

ghci> not ConTrue
If ConTrue ConFalse ConTrue

\end{verbatim}
}

%endif

\end{frame}

