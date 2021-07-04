A better example of increasing salaries with a limited budget might be to set
a fixed proportional raise, based on the total salaries across the company.
An easy way to accomplish this is to first run a query on the company to find
the salaries, and secondly to traverse the tree performing the increases on
the salaries:

\begin{code}
import CompanyDatatypes
import Data.Generics.Alloy
import Instances

increase :: Float -> Company -> Company
increase k = applyBottomUp (incS k)

incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))

totalSalary :: Company -> Float
totalSalary = sum . map (\(S s) -> s) . listifyDepth (const True)

main = print $ increase (5000 / totalSalary genCom) genCom
\end{code}%$

This code uses the \lstinline|listifyDepth| function, which is akin to SYB's
\lstinline|listify|.  Given a function of type \lstinline|s -> Bool|,
\lstinline|listifyDepth| returns a list of all items of type \lstinline|s|
that result in \lstinline|True|.  Here, all salaries are needed so
\lstinline|const True| is the suitable definition.  \lstinline|listifyDepth|
is implemented using a traversal with the \lstinline|State| monad, and this
method can be used to implement other similary query operations.

