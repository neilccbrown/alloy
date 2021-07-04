The previous example applied the salary increase to all employees in the
company.  Often, traversals need to be more selective, based on nodes further
up (i.e. closer to the root) in the tree.  We will now consider how to
increase the salary of all employees except those that are anywhere in the
research department.  We must bear in mind that departments may contain
departments:

\begin{code}
{-# LANGUAGE TypeOperators #-}
import CompanyDatatypes
import Data.Generics.Alloy
import Instances

increaseAllButResearch :: Float -> Company -> Company
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

There are several new concepts here.  The main concept is the opset (short for
operations set).  An opset is built using the \lstinline|:-| constructor in a
\textit{cons}-fashion, terminated by the \lstinline|baseOp| function (of
\lstinline|BaseOp| type).  The type of an opset mirrors its construction,
showing that it is an opset on the two types \lstinline|Dept| and
\lstinline|Salary|.  Usually the type of an opset can be inferred and thus it
is a matter of style whether to include the type.

An opset is used primarily with two functions: \lstinline|makeRecurse| and
\lstinline|makeDescend|.  Broadly, \lstinline|makeRecurse| is used to
begin a traversal, and \lstinline|makeDescend| is used to continue it; \lstinline|makeRecurse| applies the
operations to all the largest types (the first ones encountered in a
depth-first search) it can find, potentially including the argument you have
given it -- in contrast, \lstinline|makeDescend| begins with the type's
children.
The \lstinline|increaseAllButResearch| function uses \lstinline|makeRecurse|
to begin the traversal of the company.  However, \lstinline|doDept| must use
\lstinline|makeDescend| in order to operate on the children of the
\lstinline|Dept|.  If \lstinline|doDept| had used \lstinline|makeRecurse|, an
infinite loop would have resulted from \lstinline|doDept| continually being
applied to the same department.  

The function works by examining the department name.  If the name is
\lstinline|"Research"|, the department is returned unaltered (as we do not
wish to alter any employees' salaries in research, even in sub-departments).
Otherwise, the traversal continues across the department, looking for further
sub-departments, and also salaries to increase as before.
