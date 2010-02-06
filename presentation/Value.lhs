%include polycode.fmt
%include code.fmt

%if style == newcode

> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -F -pgmF lhs2tex -optF --pre #-}

> module Value where

%endif

% -----------------------------------------------------------------------------

\begin{frame}

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

% -----------------------------------------------------------------------------

\begin{frame}

> test  :: (NumC j, ListC j, Eq j Num, BoolC j, FunC j, MaybeC j)
>       => j (Num -> Num)
> test  =  lam (\x  ->  sum (replicate 3 (2 * 8) ++ replicate 3 8)
>                   *   maybe 4 (*8) (just (x - 2)))

\pause
\Large{
\textcolor{cmnt}{
\begin{verbatim}
ghci> (runHaskell test) 3
576
\end{verbatim}
}
}

\end{frame}

\begin{frame}

> test  :: JavaScript (Num -> Num)
> test  =  lam (\x  ->  sum (replicate 3 (2 * 8) ++ replicate 3 8)
>                   *   maybe 4 (*8) (just (x - 2)))

\pause
\Large{
\textcolor{cmnt}{
\begin{verbatim}
ghci> Js.compiler test >>=
      writeFile "test.js"
\end{verbatim}
}
}

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}

\Huge{\textcolor{strlit}{\tt{JavaScript!}}}

\tiny{
\textcolor{cmnt}{
\begin{verbatim}
var mul = function (v1) { return function (v2) { return v1 * v2; }; }; var fix = function (v1) { return fix
= arguments.callee, v1(function (i) { return fix(v1)(i) }); }; var list = function (v1) { return function
(v2) { return function (v3) { return v3.nil ? v1 : v2(v3.head)(v3.tail); }; }; }; var add = function (v1) {
return function (v2) { return v1 + v2; }; }; var bool = function (v1) { return function (v2) { return
function (v3) { return v3 ? v1(/*force*/) : v2(/*force*/); }; }; }; var cons = function (v1) { return
function (v2) { return { head : v1, tail : v2 }; }; }; var sub = function (v1) { return function (v2) {
return v1 - v2; }; }; var eq = function (v1) { return function (v2) { return v1 == v2; }; }; var maybe =
function (v1) { return function (v2) { return function (v3) { return v3.nothing ? v1 : v2(v3.just); }; }; };
var just = function (v1) { return { just : v1 }; }; var c10_11 = list(0); var c10_12 = function (v1) {
return function (v2) { return c10_11(function (v3) { return function (v4) { return add(v3)(v1(v4)); }; })
(v2); }; }; var c10_13 = fix(c10_12); var c10_14 = function (v1) { return function (v2) { return v1; }; };
var c10_15 = c10_14({ nil : 1 }); var c10_16 = function (v1) { return c10_15(v1); }; var c10_17 = bool(
c10_16); var c10_19 = cons(8); var c10_20 = function (v1) { return function (v2) { return c10_17(function
(v3) { return c10_14(c10_19(v1(sub(v2)(1))))(v3); })(eq(v2)(0)); }; }; var c10_21 = fix(c10_20); var c10_22
= c10_21(3); var c10_23 = list(c10_22); var c10_24 = function (v1) { return function (v2) { return c10_23(
function (v3) { return function (v4) { return cons(v3)(v1(v4)); }; })(v2); }; }; var c10_25 = fix(c10_24);
var c10_31 = mul(2); var c10_32 = c10_31(8); var c10_33 = cons(c10_32); var c10_34 = function (v1) {
return function (v2) { return c10_17(function (v3) { return c10_14(c10_33(v1(sub(v2)(1))))(v3); })(eq(v2)
(0)); }; }; var c10_35 = fix(c10_34); var c10_36 = c10_35(3); var c10_37 = c10_25(c10_36); var c10_38 =
c10_13(c10_37); var c10_39 = mul(c10_38); var c10_40 = maybe(4); var c10_41 = function (v1) { return
mul(v1)(8); }; var c10_42 = c10_40(c10_41); var __main = function (v1) { return c10_39(c10_42(just(sub(v1)
(2)))); };
\end{verbatim}
}
}

\end{frame}

\begin{frame}

\Huge{
\textcolor{cmnt}{
\begin{verbatim}
   alert(__main(3));
\end{verbatim}
}
}

\end{frame}

