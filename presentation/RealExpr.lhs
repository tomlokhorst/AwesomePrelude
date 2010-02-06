%include polycode.fmt
%include code.fmt

%if style == newcode

> {-# OPTIONS_GHC -F -pgmF lhs2tex -optF --pre #-}

> module Expr where

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

> WhiteSpace
> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j BoolD -> j r

}
\only<2>{

> WhiteSpace
> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j BoolD -> j r

> data Maybe a
> 
> class MaybeC j where
}
\only<3>{

> WhiteSpace
> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j BoolD -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)

}
\only<4>{

> WhiteSpace
> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j BoolD -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)
>   just     :: j a -> j (Maybe a)

}
\only<5>{

> WhiteSpace
> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j BoolD -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)
>   just     :: j a -> j (Maybe a)
>   maybe    :: j r -> (j a -> j r) -> j (Maybe a) -> j r

}
\only<6>{

> WhiteSpace
> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j BoolD -> j r

> data Maybe a
> 
> class MaybeC j where
>   nothing  :: j (Maybe a)
>   just     :: j a -> j (Maybe a)
>   maybe    :: j r -> (j a -> j r) -> j (Maybe a) -> j r

> class ListC j where

}
\only<7>{

> WhiteSpace
> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j BoolD -> j r

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

> WhiteSpace
> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j BoolD -> j r

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

> WhiteSpace
> data Bool
> 
> class BoolC j where
>   false  :: j Bool
>   true   :: j Bool
>   bool   :: j r -> j r -> j BoolD -> j r

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

test

%if style /= newcode

> if 2 + 3 == 5
> then  1
> else  0

\pause


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

