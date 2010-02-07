%include polycode.fmt
%include code.fmt

%if style == newcode

> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -F -pgmF lhs2tex -optF --pre #-}

> module Conclusion where

%endif

% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{This prototype}

\begin{itemize}

\item Abstract away from concrete datatypes.
\item Abstract away from functions.
\item Replace with type classes.\linebreak

\item Different instances for different computational contexts.
\item Functions look similar.
\item Types get complicated.\linebreak

\item Plain lazy and purely functional Haskell.
\item Purely functional strict JavaScript.
\item Functional reactive JavaScript.

\end{itemize}

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{Current problems}

\begin{itemize}

\item Explicit lifting of function application and recursion.
\item Type signatures with big contexts.
\item No sugar for pattern matching, let bindings, if-then-else.

\end{itemize}

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{Future work}

\begin{itemize}

\item Syntactic front-end.

\begin{item}
  Additional computational contexts:
  \begin{itemize}
  \item Strict Haskell
  \item Functional Reactive Haskell
  \item Profiling support
  \item C, Objective-C, C\tt{\#}, etc...
  \end{itemize}
\end{item}

\item Generic derivation of instances.

\end{itemize}

\end{frame}

