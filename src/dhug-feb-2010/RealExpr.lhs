%include polycode.fmt
%include code.fmt

%if style == newcode

> {-# OPTIONS_GHC -F -pgmF lhs2tex -optF --pre #-}

> module RealExpr where

%endif


\begin{frame}

%if style /= newcode

\begin{overlayarea}{\textwidth}{0.5\textheight}
\only<1>{

> data Expr where
>   Con       ::  Int   ->  Expr
>   Add       ::  Expr  ->  Expr  ->  Expr 
>   Mul       ::  Expr  ->  Expr  ->  Expr 
>   ConFalse  ::  Expr 
>   ConTrue   ::  Expr 
>   Eq        ::  Expr  ->  Expr  ->  Expr 
>   If        ::  Expr  ->  Expr  ->  Expr  ->  Expr
  
}
\only<2>{

> data Expr a where
>   Con       ::  Int        ->  Expr Int
>   Add       ::  Expr Int   ->  Expr Int  ->  Expr Int
>   Mul       ::  Expr Int   ->  Expr Int  ->  Expr Int
>   ConFalse  ::  Expr Bool
>   ConTrue   ::  Expr Bool
>   Eq        ::  Expr Int   ->  Expr Int  ->  Expr Bool
>   If        ::  Expr Bool  ->  Expr a    ->  Expr a        ->  Expr a

}  
\end{overlayarea}

%endif

\end{frame}


\begin{frame}

%if style /= newcode

\begin{overlayarea}{\textwidth}{0.5\textheight}
\only<1>{

> WhiteSpace
>
> class BoolLike b where
>   false  :: b
>   true   :: b
>   bool   :: a -> a -> b -> a

> instance BoolLike Expr where
>   false       = ConFalse
>   true        = ConTrue
>   bool x y b  = If b y x

}
\only<2>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

> instance BoolC Expr where
>   false       = ConFalse
>   true        = ConTrue
>   bool x y b  = If b y x

}
\end{overlayarea}

%endif

\end{frame}


\begin{frame}

%if style /= newcode

\begin{overlayarea}{\textwidth}{1.0\textheight}
\only<1>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

}
\only<2>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

> data Maybe a
> 
> class MaybeC j where

}
\only<3>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)

}
\only<4>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)
>   just     :: j a -> j (Maybe a)

}
\only<5>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)
>   just     :: j a -> j (Maybe a)
>   maybe    :: j r -> (j a -> j r) -> j (Maybe a) -> j r

}
\only<6>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)
>   just     :: j a -> j (Maybe a)
>   maybe    :: j r -> (j a -> j r) -> j (Maybe a) -> j r

> class ListC j where

}
\only<7>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)
>   just     :: j a -> j (Maybe a)
>   maybe    :: j r -> (j a -> j r) -> j (Maybe a) -> j r

> class ListC j where
>   nil   :: j [a]

}
\only<8>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)
>   just     :: j a -> j (Maybe a)
>   maybe    :: j r -> (j a -> j r) -> j (Maybe a) -> j r

> class ListC j where
>   nil   :: j [a]
>   cons  :: j a -> j [a] -> j [a]

}
\only<9>{

> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j Bool -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)
>   just     :: j a -> j (Maybe a)
>   maybe    :: j r -> (j a -> j r) -> j (Maybe a) -> j r

> class ListC j where
>   nil   :: j [a]
>   cons  :: j a -> j [a] -> j [a]
>   list  :: j r -> (j a -> j [a] -> j r) -> j [a] -> j r

}
\end{overlayarea}

%endif

\end{frame}


\begin{frame}

%if style /= newcode

\begin{overlayarea}{\textwidth}{0.3\textheight}
\only<1>{

> class FunC j where

}
\only<2>{

> class FunC j where
>   lam  :: (j a -> j b) -> j (a -> b)

}
\only<3>{

> class FunC j where
>   lam  :: (j a -> j b) -> j (a -> b)
>   WhiteSpace
>   app  :: j (a -> b) -> j a -> j b

}
\only<4>{

> class FunC j where
>   lam  :: (j a -> j b) -> j (a -> b)
>   fix  :: (j (a -> b) -> j (a -> b)) -> j (a -> b)
>   app  :: j (a -> b) -> j a -> j b

}
\end{overlayarea}

%endif


\end{frame}


\begin{frame}

%if style /= newcode

> foldr  :: (a -> b -> b) -> b -> [a] -> b

\pause

> foldr  :: (FunC j, ListC j) => (j a -> j b -> j b) -> j b -> j [a] -> j b
> foldr f b xs =  fix (\r -> lam (list b (\y ys -> f y (r `app` ys))))
>                 `app` xs

\pause

> jsFoldr  :: (JavaScript a -> JavaScript b -> JavaScript b) -> JavaScript b
>          -> JavaScript [a] -> JavaScript b
> jsFoldr = foldr

%endif


\end{frame}

