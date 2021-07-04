So far we have seen Alloy operating with pure functions.  Often, traversals
need to have effects.  Alloy supports effects with applicative functors, and
as a helpful common case of applicative functors: monads.  Consider the case
where we want to increase salaries in the company, until we run out of
budget.  For our example, which salaries are increased will be fairly
arbitrary (the order of the tree traversal), but such is life!  We will
maintain a remaining budget total in a state monad as we traverse.

To use effectful transformations, we must use the \lstinline|AlloyA|
type-class instead of \lstinline|Alloy|.  All of the helper functions we have
seen so far are available, with an \lstinline|A| suffix (for
\lstinline|Applicative|) and an \lstinline|M| suffix (for \lstinline|Monad|).

Here is the code for increasing the salaries up to a given budget:

\begin{code}
import CompanyDatatypes
import Data.Generics.Alloy
import Instances
import Control.Monad.State

increase :: Float -> Company -> Company
increase k c = evalState (applyBottomUpM (incS k) c) 1000

incS :: Float -> Salary -> State Float Salary
incS k (S s)
  = do budget <- get
       if diff > budget
         then return (S s)
         else do put $ budget - diff
                 return (S s')
  where
    s' = s * (1+k)
    diff = s' - s

main = print $ increase 0.1 genCom
\end{code}%$

