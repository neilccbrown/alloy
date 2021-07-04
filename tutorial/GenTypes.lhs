To generate instances, you must write a short Haskell program that uses the
\lstinline|Data.Generics.Alloy.GenInstances| module.  Here is the example for
the \lstinline|CompanyDatatypes| module:

\begin{code}
import CompanyDatatypes
import Data.Generics.Alloy.GenInstances

main :: IO ()
main = writeInstancesTo (allInstances GenWithoutOverlapped)
         [genInstance (undefined :: Company)]
         (["module Instances where"
          ,"import qualified CompanyDatatypes"
          ] ++ instanceImports)
         "Instances.hs"
\end{code}

The configuration options (the \lstinline|allInstances| call) can be ignored for
now, but we will return to them later.  This program will generate a file
named ``\verb$Instances.hs$'' which is a complete module with instances for
all the data types that can possibly be contained in the \lstinline|Company|
data type.   Note that the \lstinline|Company| datatype, and anything it
contains, must have a \lstinline|Data| instance.  This can be done
automatically in GHC by simply adding \lstinline|Typeable| and
\lstinline|Data| to the deriving clause for your data types.

You supply the header for the module yourself.  The three requirements for
Alloy are that you must import the \lstinline|Data.Generics.Alloy| module, and
(as a qualified import) the module(s) that contain the types you are
generating instances for.  If you generate all instances as we are, you must also import the \lstinline|Control.Applicative| and \lstinline|Control.Monad| modules (which we will return to later).
