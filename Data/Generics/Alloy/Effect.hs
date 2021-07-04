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

-- | The module containing the AlloyA type-class for working with effectful functions
-- (of the type @a -> m a@).  This module is an analogue to "Data.Generics.Alloy.Pure"
-- that supports functions that result in a monadic or applicative functor type.
--
-- All the functions in this module have versions for 'Applicative' and for
-- 'Monad'.  They have the same behaviour, and technically only the
-- 'Applicative' version is necessary, but since not all monads have
-- 'Applicative' instances, the 'Monad' versions are provided for convenience.
module Data.Generics.Alloy.Effect where

import Control.Applicative

-- | The Alloy type-class for effectful functions, to be used with sets of
-- operations constructed from 'BaseOpA' and ':-*'.  You are unlikely to need to
-- use 'transform' directly; instead use 'makeRecurseA'\/'makeRecurseM' and 'makeDescendA'\/'makeDescendM'.
--
-- The first parameter to the type-class is the type currently being operated
-- on, the second parameter is the set of operations to perform directly on
-- the type, and the third parameter is the set of operations to perform on
-- its children (if none of the second parameter operations can be applied).
class AlloyA t o o' where
  transformM :: Monad m => o m -> o' m -> t -> m t
  transformA :: Applicative f => o f -> o' f -> t -> f t

-- | A type representing a monadic/applicative functor modifier function that
-- applies the given ops (opT) in the given monad/functor (f) directly to the
-- given type (t).
type RecurseA f opT = forall t. AlloyA t opT BaseOpA => t -> f t

-- | Given a set of operations (as described in the 'AlloyA' type-class),
-- makes a recursive modifier function that applies the operations directly to
-- the given type, and then to its children, until it has been applied to all
-- the largest instances of that type.
makeRecurseA :: Applicative f => opT f -> RecurseA f opT
makeRecurseA ops = transformA ops baseOpA

-- | Useful equivalent of 'makeRecurseA'.
makeRecurseM :: Monad m => opT m -> RecurseA m opT
makeRecurseM ops = transformM ops baseOpA

-- | A type representing a monadic/applicative functor modifier function that
-- applies the given ops (opT) in the given monad/functor (f) to the children of the
-- given type (t).
type DescendA f opT = forall t. AlloyA t BaseOpA opT => t -> f t

-- | Given a set of operations, makes a descent modifier function that applies
-- the operation to the type's children, and further down, until it has been applied
-- to all the largest instances of that type.
makeDescendA :: Applicative f => opT f -> DescendA f opT
makeDescendA ops = transformA baseOpA ops

-- | Useful equivalent of 'makeDescendA'.
makeDescendM :: Monad m => opT m -> DescendA m opT
makeDescendM ops = transformM baseOpA ops

-- | The terminator for effectful opsets.  Note that all effectful opsets are the
-- same, and both can be used with the applicative functions or monad functions
-- in this module.  Whereas there is, for example, both 'makeRecurseA' and 'makeRecurseM',
-- there is only one terminator for the opsets, 'BaseOpA', which should be used
-- regardless of whether you use 'makeRecurseA' or 'makeRecurseM'.
data BaseOpA (m :: * -> *) = BaseOpA

-- | The function to give you an item of type 'BaseOpA'.
baseOpA :: BaseOpA m
baseOpA = BaseOpA

-- | The type that extends an opset (opT) in the given
-- monad/applicative-functor (m) to be applied to the given type (t).  This is
-- for use with the 'AlloyA' class.  A set of operations that operates
-- on @Foo@, @Bar@ and @Baz@ in the IO monad can be constructed so:
--
-- > ops :: (Foo :-* Bar :-* Baz :-* BaseOpA) IO
-- > ops = doFoo :-* doBar :-* doBaz :-* baseOpA
-- >
-- > doFoo :: Foo -> IO Foo
-- > doBar :: Bar -> IO Bar
-- > doBaz :: Baz -> IO Baz
--
-- The monad/functor parameter needs to be given when declaring an actual opset,
-- but must be omitted when using the opset as part of a type-class constraint
-- such as:
--
-- > f :: AlloyA a (Foo :-* Bar :-* Baz :-* BaseOpA) BaseOpA => a -> IO a
-- > f = makeRecurse ops
data (t :-* opT) m = (t -> m t) :-* (opT m)

infixr 7 :-*

-- | A handy synonym for a monadic/applicative opset with only one item, to use with 'AlloyA'.
type OneOpA t = t :-* BaseOpA

-- | A handy synonym for a monadic/applicative opset with only two items, to use with 'AlloyA'.
type TwoOpA s t = (s :-* t :-* BaseOpA)
