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

-- | A slightly experimental add-on for Alloy involving the idea of routes to a
-- particular part of a tree.
module Data.Generics.Alloy.Route
  (Route, routeModify, routeModifyM, routeGet, routeSet, (@->), identityRoute, routeId, routeList,
    makeRoute, routeDataMap, routeDataSet, AlloyARoute(..), BaseOpARoute(..), baseOpARoute,
      (:-@)(..), OneOpARoute, TwoOpARoute)
  where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A Route is a way of navigating to a particular node in a tree structure.
--
-- Let's say that you have some binary tree structure:
--
-- > data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)
--
-- Suppose you then have a big binary tree of integers, potentially with duplicate values,
-- and you want to be able to modify a particular integer.  You can't modify in-place,
-- because this is a functional language.  So you instead want to be able to apply
-- a modify function to the whole tree that really just modifies the particular
-- integer, deep within the tree.
--
-- To do this you can use a route:
-- 
-- > myRoute :: Route Int (BinTree Int)
--
-- You apply it as follows (for example, to increment the integer):
--
-- > routeModify myRoute (+1) myTree
--
-- This will only work if the route is valid on the given tree.
--
-- The usual way that you get routes is via the traversal functions in the module.
--
-- Another useful aspect is composition.  If your tree was in a tree of trees:
--
-- > routeToInnerTree :: Route (BinTree Int) (BinTree (BinTree Int))
--
-- You could compose this with the earlier route:
-- 
-- > routeToInnerTree @-> myRoute :: Route Int (BinTree (BinTree Int))
-- 
-- These routes are a little like zippers, but rather than building a new data
-- type to contain the zipped version and the re-use aspect, this is just a
-- simple add-on to apply a modification function in a particular part of the
-- tree.  Multiple routes can be used to modify the same tree, which is also
-- useful.
--
-- Routes support Eq, Show and Ord.  All these instances represent a route as a
-- list of integers: a route-map.  [0,2,1] means first child (zero-based), then
-- third child, then second child of the given data-type.  Routes are ordered using
-- the standard list ordering (lexicographic) over this representation.
data Route inner outer = Route [Int] (forall m. Monad m => (inner -> m inner) -> (outer -> m outer))

instance Eq (Route inner outer) where
  (==) (Route xns _) (Route yns _) = xns == yns

instance Ord (Route inner outer) where
  compare (Route xns _) (Route yns _) = compare xns yns

instance Show (Route inner outer) where
  show (Route ns _) = "Route " ++ show ns

-- | Gets the integer-list version of a route.  See the documentation of 'Route'.
routeId :: Route inner outer -> [Int]
routeId (Route ns _) = ns

-- | Given an index (zero is the first item), forms a route to that index item
-- in the list.  So for example:
--
-- > routeModify (routeList 3) (*10) [0,1,2,3,4,5] == [0,1,2,30,4,5]
-- 
routeList :: Int -> Route a [a]
routeList 0 = Route [0] (\f (x:xs) -> f x >>= (\x' -> return (x': xs)))
routeList n = Route [1] (\f (x:xs) -> f xs >>= (\xs' -> return (x:xs'))) @-> routeList (n-1)

-- | Constructs a Route to the key-value pair at the given index (zero-based) in
-- the ordered map.  Routes involving maps are difficult because Map hides its
-- internal representation.  This route secretly boxes the Map into a list of pairs
-- and back again when used.  The identifiers for map entries (as used in the integer
-- list) are simply the index into the map as passed to this function.
routeDataMap :: Ord k => Int -> Route (k, v) (Map.Map k v)
routeDataMap n = Route [n] (\f m -> let (pre, x:post) = splitAt n (Map.toList m)
  in do x' <- f x
        return $ Map.fromList $ pre ++ (x':post))

-- | Constructs a Route to the value at the given index (zero-based) in the ordered
-- set.  See the documentation for 'routeDataMap', which is nearly identical to
-- this function.
routeDataSet :: Ord k => Int -> Route k (Set.Set k)
routeDataSet n = Route [n] (\f m -> let (pre, x:post) = splitAt n (Set.toList m)
  in do x' <- f x
        return $ Set.fromList $ pre ++ (x':post))


-- | Applies a pure modification function using the given route.
routeModify :: Route inner outer -> (inner -> inner) -> (outer -> outer)
routeModify (Route _ wrap) f = runIdentity . wrap (return . f)

-- | Applies a monadic modification function using the given route.
routeModifyM :: Monad m => Route inner outer -> (inner -> m inner) -> (outer -> m
  outer)
routeModifyM (Route _ wrap) = wrap

-- | Given a route, gets the value in the large data structure that is pointed
-- to by that route.
routeGet :: Route inner outer -> outer -> inner
routeGet route = flip execState undefined . routeModifyM route (\x -> put x >> return x)

-- | Given a route, sets the value in the large data structure that is pointed
-- to by that route.
routeSet :: Route inner outer -> inner -> outer -> outer
routeSet route x = routeModify route (const x)

-- | Composes two routes together.  The outer-to-mid route goes on the left hand
-- side, and the mid-to-inner goes on the right hand side to form an outer-to-inner
-- route.
(@->) :: Route mid outer -> Route inner mid -> Route inner outer
(@->) (Route outInds outF) (Route inInds inF) = Route (outInds ++ inInds) (outF
  . inF)

-- | The identity route.  This has various obvious properties:
--
-- > routeGet identityRoute == id
-- > routeSet identityRoute == const
-- > routeModify identityRoute == id
-- > identityRoute @-> route == route
-- > route @-> identityRoute == route
identityRoute :: Route a a
identityRoute = Route [] id

-- | Given the integer list of identifiers and the modification function, forms
-- a Route.  It is up to you to make sure that the integer list is valid as described
-- in the documentation of 'Route', otherwise routes constructed this way and via
-- the Alloy functions may exhibit strange behaviours when compared.
makeRoute :: [Int] -> (forall m. Monad m => (inner -> m inner) -> (outer -> m outer))
  -> Route inner outer
makeRoute = Route

-- | An extension to 'AlloyA' that adds in 'Route's.  The opsets are now parameterised
-- over both the monad/functor, and the outer-type of the route.
class AlloyARoute t o o' where
  transformMRoute :: Monad m => o m outer -> o' m outer -> (t, Route t outer) -> m t
  transformARoute :: Applicative f => o f outer -> o' f outer -> (t, Route t outer) -> f t

-- | Like 'baseOpA' but for 'AlloyARoute'.
baseOpARoute :: BaseOpARoute m outer
baseOpARoute = BaseOpARoute

-- | The type that extends an applicative/monadic opset (opT) in the given
-- functor/monad (m) to be applied to the given type (t) with routes to the
-- outer type (outer).  This is for use with the 'AlloyARoute' class.
data (t :-@ opT) m outer = ((t, Route t outer) -> m t) :-@ (opT m outer)

infixr 7 :-@

-- | The terminator for opsets with 'AlloyARoute'.
data BaseOpARoute (m :: * -> *) outer = BaseOpARoute


-- | A handy synonym for a monadic/applicative opset with only one item, to use with 'AlloyARoute'.
type OneOpARoute t = t :-@ BaseOpARoute

-- | A handy synonym for a monadic/applicative opset with only two items, to use with 'AlloyARoute'.
type TwoOpARoute s t = (s :-@ t :-@ BaseOpARoute)
