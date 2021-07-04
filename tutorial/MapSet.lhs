Alloy builds its type-classes using the \lstinline|Data| instance for the
types given to it.  If you derive \lstinline|Data| and \lstinline|Typeable|
using the built-in GHC feature, this will work fine.  One problem is that the
popular container types, \lstinline|Map| and \lstinline|Set| do not derive
\lstinline|Data| in this way and by default Alloy will fail to work with them
properly.

As a workaround, Alloy includes two special functions,
\lstinline|genMapInstance| and \lstinline|genSetInstance|.  These functions
provide a view on maps as a collection of key-value pairs, and also allow
processing of elements in sets.  We will demonstrate this with a simple
example, first some new data types:

\begin{code}
module MapSet where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Generics
import CompanyDatatypes

type Payroll = Map.Map Person Salary

type Managers = Set.Set Manager

data CompanyInfo = CompanyInfo Payroll Managers
  deriving (Typeable, Data, Show)
\end{code}


