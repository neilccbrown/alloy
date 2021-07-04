We can now use these instances to perform some operations on the data types.
First, we will define some operations to derive the \lstinline|CompanyInfo|
information, using a state monad:

\begin{code}
import CompanyDatatypes
import MapSet
import MapSetInstances
import Data.Generics.Alloy
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

companyInfo :: Company -> CompanyInfo
companyInfo c = execState (applyBottomUpM2 doEmployee doDept c)
                          (CompanyInfo Map.empty Set.empty)
  where
    doEmployee :: Employee -> State CompanyInfo Employee
    doEmployee (E p s)
      = do modify $ \(CompanyInfo es ms) ->
             CompanyInfo (Map.insert p s es) ms
           return (E p s)
    
    doDept :: Dept -> State CompanyInfo Dept
    doDept d@(D _ m _)
      = do modify $ \(CompanyInfo es ms) ->
             CompanyInfo es (Set.insert m ms)
           return d
\end{code}

We can then perform further operations on the \lstinline|CompanyInfo| type.
For example, we can increase the salary of all employees with the letter `o'
in their name:

\begin{code}
incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))

increaseOs :: Float -> CompanyInfo -> CompanyInfo
increaseOs k = applyBottomUp inc
  where
    inc :: (Person, Salary) -> (Person, Salary)
    inc (P n a, s)
      | 'o' `elem` n = (P n a, incS k s)
      | otherwise = (P n a, s)

main = print $ increaseOs 0.1 $ companyInfo genCom
\end{code}

Notice how we define the function to work on key-value pairs in order to
process the map entries.  If you wish to process the map itself differently,
you can define an operation on the map; the map instances we use here are
particularly useful for descending into maps (for example if the value in a
map can contain types you wish to process).
