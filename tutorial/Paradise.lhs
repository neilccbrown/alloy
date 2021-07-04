Having generated the instances, we can now write the paradise benchmark, that
modifies all the salaries in the company.  Since we are operating on all
instances of a particular data-type, we can use the helper function
\lstinline|applyBottomUp| (akin to \lstinline|everywhere| in SYB):

\begin{code}
import CompanyDatatypes
import Data.Generics.Alloy
import Instances

increase :: Float -> Company -> Company
increase k = applyBottomUp (incS k)

incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))

main = print $ increase 0.1 genCom
\end{code}%$

This is the most basic use of Alloy.  There is also an
\lstinline|applyBottomUp2| function that takes two functions operating on
distinct types, and applies both of them throughout the data structure.
