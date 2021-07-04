As another example we will consider how to find the employee(s) with the
lowest salary in the company and increase just their salary.  This could be
done with a two-pass query, first finding the lowest salary, and second
traversing the entire tree to increment all employees with a matching salary.
We instead use this example to demonstrate routes, an experimental zipper-like feature.

\begin{code}
import CompanyDatatypes
import Data.Generics.Alloy
import Instances
import Control.Monad.State

increase :: Float -> Route Salary Company -> Company -> Company
increase k r = routeModify r (incS k)

incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))

findMin :: Company -> [Route Salary Company]
findMin c = snd $ execState (applyBottomUpMRoute minSalary c) (Nothing, [])
  where
    minSalary :: (Salary, Route Salary Company)
                   -> State (Maybe Float, [Route Salary Company]) Salary
    minSalary (S s, r)
      = do (curMin, rs) <- get
           case fmap (compare s) curMin of
             Nothing -> put (Just s, [r])
             Just LT -> put (Just s, [r])
             Just EQ -> put (curMin, r : rs)
             Just GT -> return ()
           return (S s)

main = print $ foldr (increase 0.1) genCom (findMin genCom)
\end{code}%$

The route is a path into a tree of type \lstinline|Company|, to an item of
type \lstinline|Salary|.  This route can be used for getting, setting or
modifying, when applied to the same tree that it was derived from.  This means
that the whole tree does not need to be traversed again to alter a couple of
salaries, which can be a useful saving with large trees.

This strategy is vaguely similar to zippers, but uses mutation rather than any
more complex manipulations.  Multiple routes can be used to modify the
same tree, as long as the final nodes are disjoint (i.e. one does not contain
another).
