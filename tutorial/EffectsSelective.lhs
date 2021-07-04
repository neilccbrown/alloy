We can now put together two of our previous examples, to selectively increase
the salary of all those not in the research department, up to a given budget:


\begin{code}
{-# LANGUAGE TypeOperators #-}
import CompanyDatatypes
import Data.Generics.Alloy
import Instances
import Control.Monad.State

increaseAllButResearch :: Float -> Company -> Company
increaseAllButResearch k c = evalState (makeRecurseM ops c) 15000
  where
    ops :: (Dept :-* Salary :-* BaseOpA) (State Float)
    ops = doDept :-* incS k :-* baseOpA

    doDept :: Dept -> State Float Dept
    doDept d@(D name _ _)
      | name == "Research" = return d
      | otherwise = makeDescendM ops d

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

main = print $ increaseAllButResearch 0.1 genCom
\end{code}%$

The changes in the \lstinline|increaseAllButResearch| function are that the
\lstinline|:-| constructor has become \lstinline|:-*| in the effectful
version, and similarly \lstinline|baseOp| has become \lstinline|baseOpA|.  The
terminator is oblivious to whether the effect in question is an applicative
functor or a monad, hence there is only the \lstinline|A|-suffixed version.
The opset is then parameterised by the monad in question (the bracketing in
the type of \lstinline|ops| is important).

Apart from these small textual changes, it can be seen that the code is
roughly the same.
