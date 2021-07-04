-- Alloy.
-- Copyright (c) 2008-2009, University of Kent.
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--  * Neither the name of the University of Kent nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- | A module of helper functions for use with Alloy.  Most of the functions
-- have versions for pure functions (without suffix), applicative functors (A
-- suffix) and monads (M suffix) and sometimes the monadic version again with routes.
-- Generally, only the pure version is documented.  The key functions you are likely
-- to need (or their suffixed versions) are 'applyBottomUp' and 'applyBottomUp2',
-- and 'listifyDepth'.
module Data.Generics.Alloy.Schemes where

import Control.Applicative
import Control.Monad.State

import Data.Generics.Alloy.Pure
import Data.Generics.Alloy.Effect
import Data.Generics.Alloy.Route

-- * Functions to easily apply transformations throughout a data structure

-- | Given a function that applies to a particular type (@s@), automatically
-- applies that function to every instance of @s@ in a larger structure of
-- type @t@, performing the transformations in a bottom-up fashion.  It does a
-- depth first traversal in order of a constructor's children, descending
-- first and applying the function afterwards on the way back up.
--
-- This is equivalent to SYB's everywhere function, as it applies the function
-- everywhere it can throughout the data structure.  The function will not be applied
-- to the results of your transformation, so the function cannot end up in infinite
-- loop (unless the value you apply the function to is infinite!).
applyBottomUp :: (Alloy t (OneOp s) BaseOp,
                  Alloy s BaseOp (OneOp s)) =>
                 (s -> s) -> t -> t
applyBottomUp f = makeRecurse ops
  where
    ops = makeBottomUp ops f :- baseOp

applyBottomUpA :: (AlloyA t (OneOpA s) BaseOpA,
                   AlloyA s BaseOpA (OneOpA s), Applicative f) =>
                  f (s -> s) -> t -> f t
applyBottomUpA f = makeRecurseA ops
  where
    ops = makeBottomUpA ops f :-* baseOpA

applyBottomUpM :: (AlloyA t (OneOpA s) BaseOpA,
                   AlloyA s BaseOpA (OneOpA s), Monad m) =>
                  (s -> m s) -> t -> m t
applyBottomUpM f = makeRecurseM ops
  where
    ops = makeBottomUpM ops f :-* baseOpA

applyBottomUpMRoute :: (AlloyARoute t (OneOpARoute s) (BaseOpARoute),
                        AlloyARoute s (BaseOpARoute) (OneOpARoute s),
                        Monad m) =>
                       ((s, Route s t) -> m s) -> t -> m t
applyBottomUpMRoute f x = transformMRoute ops baseOpARoute (x, identityRoute)
  where
    ops = makeBottomUpMRoute ops f :-@ baseOpARoute


-- | As 'applyBottomUp', but applies both functions whereever it can in the
-- data structure.  It is very important that @sA@ is not the same type as
-- @sB@ -- odd results will occur if they are the same type.  It is perfectly
-- valid for @sA@ to contain @sB@ or vice versa; in this case, the smaller
-- type will be processed first (as this is a bottom-up traversal) and the
-- larger type processed later on in the ascent (towards the root) of the
-- tree.
applyBottomUp2 :: (Alloy t (TwoOp sA sB) BaseOp,
                  Alloy sA BaseOp (TwoOp sA sB),
                  Alloy sB BaseOp (TwoOp sA sB)) =>
                 (sA -> sA) -> (sB -> sB) -> t -> t
applyBottomUp2 fA fB = makeRecurse ops
  where
    ops = makeBottomUp ops fA :- makeBottomUp ops fB :- baseOp

applyBottomUpA2 :: (AlloyA t (TwoOpA sA sB) (BaseOpA),
                    AlloyA sA (BaseOpA) (TwoOpA sA sB),
                    AlloyA sB (BaseOpA) (TwoOpA sA sB),
                    Applicative f
                   ) =>
                   f (sA -> sA) -> f (sB -> sB) -> t -> f t
applyBottomUpA2 fA fB = makeRecurseA ops
  where
    ops = makeBottomUpA ops fA :-* makeBottomUpA ops fB :-* baseOpA

applyBottomUpM2 :: (AlloyA t (TwoOpA sA sB) (BaseOpA),
                    AlloyA sA (BaseOpA) (TwoOpA sA sB),
                    AlloyA sB (BaseOpA) (TwoOpA sA sB),
                    Monad m
                   ) =>
                   (sA -> m sA) -> (sB -> m sB) -> t -> m t
applyBottomUpM2 fA fB = makeRecurseM ops
  where
    ops = makeBottomUpM ops fA :-* makeBottomUpM ops fB :-* baseOpA

applyBottomUpMRoute2 :: (AlloyARoute t (TwoOpARoute sA sB) (BaseOpARoute),
                        AlloyARoute sA (BaseOpARoute) (TwoOpARoute sA sB),
                        AlloyARoute sB (BaseOpARoute) (TwoOpARoute sA sB),
                        Monad m) =>
                       ((sA, Route sA t) -> m sA)
                       -> ((sB, Route sB t) -> m sB)
                       -> t -> m t
applyBottomUpMRoute2 fA fB x = transformMRoute ops baseOpARoute (x, identityRoute)
  where
    ops = makeBottomUpMRoute ops fA :-@ makeBottomUpMRoute ops fB :-@ baseOpARoute


-- * Listify functions that return lists of items that satisfy given criteria

-- | Given a function that examines a type @s@ and gives an answer (True to include
-- the item in the list, False to drop it), finds all items of type @s@ in some
-- larger item (of type @t@) that satisfy this function, listed in depth-first
-- order.
listifyDepth :: (AlloyA t (OneOpA s) BaseOpA
                ,AlloyA s BaseOpA (OneOpA s)) => (s -> Bool) -> t -> [s]
-- We use applyBottomUp because we are prepending to the list.  If we prepend from
-- the bottom up, that's the same as appending from the top down, which is what
-- this function is meant to be doing.
listifyDepth qf = flip execState [] . applyBottomUpM qf'
  where
    qf' x = if qf x then modify (x:) >> return x else return x

listifyDepthRoute :: (AlloyARoute t (OneOpARoute s) (BaseOpARoute)
                     ,AlloyARoute s (BaseOpARoute) (OneOpARoute s))
                     => ((s, Route s t) -> Bool) -> t -> [(s, Route s t)]
listifyDepthRoute qf = flip execState [] . applyBottomUpMRoute qf'
  where
    qf' x = if qf x then modify (x:) >> return (fst x) else return (fst x)

-- * Check functions to apply monadic checks throughout a data structure

-- | Given a monadic function that operates on items of type @s@ (without modifying
-- them), applies the function to all items of types @s@ within an item of type
-- @t@, in depth-first order.
--
-- This can be used, for example, to perform checks on items in an error monad,
-- or to accumulate information in a state monad, or to print out the structure
-- in a writer or IO monad.
checkDepthM :: (Monad m, AlloyA t (OneOpA s) BaseOpA
                       , AlloyA s BaseOpA (OneOpA s)) => (s -> m ()) -> t -> m ()
checkDepthM f x = applyBottomUpM (\x -> f x >> return x) x >> return ()

checkDepthM2 :: (Monad m, AlloyA t (TwoOpA r s) (BaseOpA)
                        , AlloyA r (BaseOpA) (TwoOpA r s)
                        , AlloyA s (BaseOpA) (TwoOpA r s)
                        ) =>
  (r -> m ()) -> (s -> m ()) -> t -> m ()
checkDepthM2 f g x = applyBottomUpM2 (\x -> f x >> return x)
                                     (\y -> g y >> return y) x >> return ()




-- * Adding traversal to modifiers

-- | Given a set of operations and a modifier function, augments that modifier
-- function to first descend into the value before then applying the modifier function.
--  This can be used to perform a bottom-up depth-first traversal of a structure
-- (see the implementation of 'applyBottomUp').
--
-- You are unlikely to need these functions much yourself; either use 'applyBottomUp'
-- and similar to apply a function everywhere, or if you need more fine-grained
-- control over the descent, it is usually better to handle the descent in your
-- own functions.
makeBottomUp :: Alloy t BaseOp opT => opT -> (t -> t) -> t -> t
makeBottomUp ops f v = f (makeDescend ops v)

makeBottomUpA :: (AlloyA t BaseOpA opT, Applicative f) => opT f -> f (t -> t) -> t -> f t
makeBottomUpA ops f v = f <*> makeDescendA ops v

makeBottomUpM :: (AlloyA t BaseOpA opT, Monad m) => opT m -> (t -> m t) -> t -> m t
makeBottomUpM ops f v = makeDescendM ops v >>= f

makeBottomUpMRoute :: (Monad m, AlloyARoute t BaseOpARoute opT) =>
  opT m outer -> ((t, Route t outer) -> m t) -> (t, Route t outer) -> m t
makeBottomUpMRoute ops f (v, r)
  = do v' <- transformMRoute baseOpARoute ops (v, r)
       f (v', r)



