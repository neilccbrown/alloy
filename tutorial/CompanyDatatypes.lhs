Below are some sample data types, originally created by Ralf L\"ammel as part
of the paradise benchmark.  They are taken directly from
\url{http://www.cs.vu.nl/boilerplate/testsuite/paradise/CompanyDatatypes.hs}.
We will use them for our first few examples of using Alloy.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
module CompanyDatatypes where

import Data.Generics

-- The organisational structure of a company

data Company  = C [Dept]               deriving (Eq, Ord, Show, Typeable, Data)
data Dept     = D Name Manager [Unit]  deriving (Eq, Ord, Show, Typeable, Data)
data Unit     = PU Employee | DU Dept  deriving (Eq, Ord, Show, Typeable, Data)
data Employee = E Person Salary        deriving (Eq, Ord, Show, Typeable, Data)
data Person   = P Name Address         deriving (Eq, Ord, Show, Typeable, Data)
data Salary   = S Float                deriving (Eq, Ord, Show, Typeable, Data)
type Manager  = Employee
type Name     = String
type Address  = String

-- An illustrative company
genCom :: Company
genCom = C [D "Research" laemmel [PU joost, PU marlow],
            D "Strategy" blair   []]

-- A typo for the sake of testing equality;
-- (cf. lammel vs. laemmel)
genCom' :: Company
genCom' = C [D "Research" lammel [PU joost, PU marlow],
             D "Strategy" blair   []]

lammel, laemmel, joost, blair :: Employee
lammel  = E (P "Lammel" "Amsterdam") (S 8000)
laemmel = E (P "Laemmel" "Amsterdam") (S 8000)
joost   = E (P "Joost"   "Amsterdam") (S 1000)
marlow  = E (P "Marlow"  "Cambridge") (S 2000)
blair   = E (P "Blair"   "London")    (S 100000)

-- Some more test data
person1 = P "Lazy" "Home"
dept1   = D "Useless" (E person1 undefined) []
\end{code}

