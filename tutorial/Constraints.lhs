So far, we have always used Alloy on known, definite types.  When you do this,
no type-class constraints are required as the compiler can go and find the
type-class instances for the definite types.  If you want to operate on
parameterised types, you will need to manually add some type-class
constraints.  In essence, you will need to copy the type-class constraints
from any Alloy function you make use of, such as makeDescend, applyBottomUp,
etc, that involves the parameterised type.  You can see all the constraints in
the documentation.  We will re-use our previous example to demonstrate:

\begin{code}
{-# LANGUAGE TypeOperators #-}
import CompanyDatatypes
import Data.Generics.Alloy
import Instances

increaseAllButResearch :: Alloy a (Dept :- Salary :- BaseOp) BaseOp =>
  Float -> a -> a
increaseAllButResearch k = makeRecurse ops
  where
    ops :: Dept :- Salary :- BaseOp
    ops = doDept :- incS k :- baseOp

    doDept :: Dept -> Dept
    doDept d@(D name _ _)
      | name == "Research" = d
      | otherwise = makeDescend ops d

incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))

main = print $ increaseAllButResearch 0.1 genCom
\end{code}%$

The extra constraint included is taken from \lstinline|makeRecurse|.  The
first parameter of the \lstinline|Alloy| type-class is the type that the
operation (\lstinline|makeRecurse|) is applied to.  For
\lstinline|makeRecurse| the second operation set is full and the third is
empty; for \lstinline|makeDescend| the reverse would be true.  We only need
include the constraint for \lstinline|makeRecurse|, and not
\lstinline|makeDescend| because the former operates on \lstinline|a| whereas
the latter here acts on a definite type, with a definite opset.

