%include polycode.fmt
%include code.fmt

%if style == newcode

> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -F -pgmF lhs2tex -optF --pre #-}

> module Value where

%endif

% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{JAVASCRIPT AST}

> type Nm = String

> data JavaScript a where
>   Con   :: Nm                                    -> JavaScript a
>   Prim  :: ([Nm] -> Nm) -> [Nm]                  -> JavaScript a
>   App   :: JavaScript (a -> b) -> JavaScript a   -> JavaScript b
>   Lam   :: (JavaScript a -> JavaScript b)        -> JavaScript (a -> b)
>   Var   :: Nm                                    -> JavaScript a
>   Name  :: Nm -> JavaScript a                    -> JavaScript a

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{INSTANCES}

> instance FunC JavaScript where
>   lam  f    = Lam  f
>   app  f g  = App  f g

> instance BoolC JavaScript where
>
>   -- constructors:
>   true   = Con "true"
>   false  = Con "false"
>
>   -- destructor:
>   bool x y z  = fun3 "bool"
>     (\[e, t, b] -> concat [b,"?",t,"():",e,"()"])
>     (lam (const x)) (lam (const y)) z

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}

> instance ListC JavaScript where
>
>   -- constructors:
>   nil   = Con "{nil:1}"
>   cons  = fun2 "cons"
>     (\[x, xs] -> concat ["{head:",x,",tail:",xs,"}"])
>
>   -- destructor:
>   list b f = fun3 "list"
>     (\[n, c, xs] -> concat
>        [xs,".nil?",n,":",c,"(",xs,".head)(",xs,".tail)"]
>     ) b (lam2 f)

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{COMPILER PIPELINE}

> type a :-> b = Kleisli IO a b
> type Code = String

> compiler :: JavaScript a -> Code
> compiler = runKleisli
>   $ ( Lambdas.instantiate      :: JavaScript a   :-> Expression     )
>   . ( Defs.lift                :: Expression     :-> Definitions    )
>   . ( Defs.eliminiateDoubles   :: Definitions    :-> Definitions    )
>   . ( FreeVars.annotateDefs    :: Definitions    :-> DefinitionsFV  )
>   . ( ClosedApplications.lift  :: DefinitionsFV  :-> Definitions    )
>   . ( Parameters.reindex       :: Definitions    :-> Definitions    )
>   . ( CommonDefs.eliminate     :: Definitions    :-> Definitions    )
>   . ( Defs.dump                :: Definitions    :-> Code           )

\end{frame}

