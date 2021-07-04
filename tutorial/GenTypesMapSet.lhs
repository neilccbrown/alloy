We will then need to generate some instances:

\begin{code}
import CompanyDatatypes
import MapSet
import Data.Generics.Alloy.GenInstances

main :: IO ()
main = writeInstancesTo (allInstances GenWithoutOverlapped)
         [genInstance (undefined :: Company)
         ,genInstance (undefined :: CompanyInfo)
         ,genMapInstance (undefined :: Person) (undefined :: Salary)
         ,genSetInstance (undefined :: Manager)]
         (["module MapSetInstances where"
          ,"import qualified CompanyDatatypes"
          ,"import qualified MapSet"
          ] ++ instanceImportsMapSet)
         "MapSetInstances.hs"
\end{code}

This is similar to our previous code for generating instances.  We call
\lstinline|genInstance| for \lstinline|Company| and \lstinline|CompanyInfo|
(neither contains the other, but between them they both contain all the data
types).  We call \lstinline|genMapInstance| for our map, passing the key and
value types as parameters, and similarly we call \lstinline|genSetInstance|.
Finally, we use \lstinline|instanceImportsMapSet| instead of \lstinline|instanceImports|.
