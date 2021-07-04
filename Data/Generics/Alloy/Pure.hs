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

-- | The module containing the Alloy type-class for working with pure functions
-- (of the type @a -> a@).
module Data.Generics.Alloy.Pure where

-- | The Alloy type-class for pure functions, to be used with sets of
-- operations constructed from 'BaseOp' and ':-'.  You are unlikely to need to
-- use 'transform' directly; instead use 'makeRecurse' and 'makeDescend'.
--
-- The first parameter to the type-class is the type currently being operated
-- on, the second parameter is the set of operations to perform directly on
-- the type, and the third parameter is the set of operations to perform on
-- its children (if none of the second parameter operations can be applied).
class Alloy t o o' where
  transform :: o -> o' -> t -> t

-- | A type representing a modifier function that applies the given ops
-- (opT) directly to the given type (t).
type Recurse opT = forall t. Alloy t opT BaseOp => t -> t

-- | Given a set of operations, makes a modifier function that applies the
-- operations directly to the given type, and then to its children, until it
-- has been applied to all the largest instances of that type.
makeRecurse :: opT -> Recurse opT
makeRecurse ops = transform ops baseOp

-- | A type representing a modifier function that applies the given ops
-- (opT) to the children of the given type (t).
type Descend opT = forall t. Alloy t BaseOp opT => t -> t

-- | Given a set of operations, makes a descent modifier function that applies
-- the operation to the type's children, and further down, until it has been applied
-- to all the largest instances of that type.
makeDescend :: opT -> Descend opT
makeDescend ops = transform baseOp ops

-- | The type of the empty set of pure operations.
data BaseOp = BaseOp

-- | The function giving the empty set of pure operations.
baseOp :: BaseOp
baseOp = BaseOp

-- | The type that extends an opset (opT) to be applied to the given type (t).
-- This is for use with the 'Alloy' type-class.  A set of operations that operates
-- on @Foo@, @Bar@ and @Baz@ can be constructed so:
--
-- > ops :: Foo :- Bar :- Baz :- BaseOp
-- > ops = doFoo :- doBar :- doBaz :- baseOp
-- >
-- > doFoo :: Foo -> Foo
-- > doBar :: Bar -> Bar
-- > doBaz :: Baz -> Baz
data t :- opT = (t -> t) :- opT

infixr 7 :-

-- | A handy synonym for an opset with only one item, to use with 'Alloy'.
type OneOp t = t :- BaseOp

-- | A handy synonym for an opset with only two items, to use with 'Alloy'.
type TwoOp s t = s :- t :- BaseOp

