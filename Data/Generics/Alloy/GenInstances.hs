-- Alloy.
-- Copyright (c) 2008-2009, 2012, 2016 University of Kent.
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

-- | A module containing code to generate instances of the Alloy class for
-- you.
--
-- Generating Alloy instances by hand would be laborious, complex and error-prone.
--  This module provides instance generation, based on the Scrap Your Boilerplate ("Data.Generics")
-- generics that have built-in support in GHC.  So you should just need to add
--
-- > deriving (Data, Typeable)
--
-- after all your data-types, then use the functions in this module to generate
-- some Haskell code with instances of the Alloy classes.  The simplest functions
-- for doing this are 'writeInstances' and 'writeInstancesTo'.  The tutorial has
-- examples of using this module.
--
-- You do not even have to modify the definitions of your data-types if you are
-- using GHC 6.8.2 or later, you can simply add these lines in your module for
-- generating the instances (assuming the data-type is not hidden during import):
--
-- > deriving instance Typeable Foo
-- > deriving instance Data Foo
--
-- That technique, and in fact this module as a whole generates orphan instances.
--  This is generally advised against in Haskell, but it should not cause any problems
-- here.
--
-- The primary drawback of Alloy's approach is that it can generate a lot of
-- type-class instances (generally, the square of the number of types).  There
-- are two ways to control this explosion.  Using 'GenWithOverlapped' tends to
-- halve the number of instances, at the cost of using a GHC extension.  If
-- you need instances for more than one of 'Alloy', 'AlloyA' and
-- 'AlloyARoute', it is possible to define one based on another, and thus
-- avoid an entire set of instances altogether.  See the alloy-proxy-fd
-- package on Hackage for more details.
module Data.Generics.Alloy.GenInstances
  (writeInstances, writeInstancesTo,
   justPure, allInstances, instanceImports, instanceImportsMapSet, instanceImportsVector,
   GenInstance, genInstance, genMapInstance, genSetInstance, genInstances, languageExtras,
   genVectorInstance,
   GenOverlappedOption(..), GenClassOption(GenOneClass), GenInstanceConfig(..)) where

import Control.Monad.State (StateT, execStateT, get, liftIO, liftM, modify, when)
import Data.Char (isAlphaNum, ord)
import Data.Generics (Data, Constr, Typeable, dataTypeConstrs, dataTypeOf, fromConstr, gmapQ, isAlgType)
import Data.List (find, intersperse, nub, sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Set as Set
import Data.Typeable as Ty (Proxy(..), TyCon, TypeRep, splitTyConApp, tyConModule, typeRep)
import qualified Data.Vector as Vector

-- | The option controlling whether the generated instances can be overlapped.
--  If you choose 'GenWithOverlapped' many less instances (around half, in our
--  experience) will be generated, but you must enable the
--  overlapping-instances flag in GHC (-XOverlappingInstances in GHC 6.8 and
--  6.10) when compiling the instances.
data GenOverlappedOption = GenWithOverlapped | GenWithoutOverlapped
  deriving (Eq, Read, Show)

-- The option controlling whether the generated instances have one class per
-- type, or just generate instances of the primary Alloy class.  Having one
-- class per type seems to compile faster on GHC, but can give less clear error messages
-- due to the name munging that goes on.

-- | For now, this option has only one setting.
data GenClassOption
  = GenClassPerType
  | GenOneClass
  | GenSlowDelegate -- ^ This is only for benchmarking purposes.  Do not use.
  deriving (Eq, Read, Show)

data GenInstanceConfig = GenInstanceConfig
  { genOverlapped :: GenOverlappedOption
  , genClass :: GenClassOption
  , genPure :: Bool
  , genEffect :: Bool
  , genRoute :: Bool
  } deriving (Eq, Read, Show)

-- | Constructs a configuration that just generates instances for the 'Alloy' type-class
-- (not 'AlloyA' or 'AlloyARoute').
justPure :: GenOverlappedOption -> GenInstanceConfig
justPure ov = GenInstanceConfig ov GenOneClass True False False

-- | Constructs instances for all the type-classes: 'Alloy', 'AlloyA' and 'AlloyARoute'.
--  This may be quite a lot, see the documentation at the top of this file.
allInstances :: GenOverlappedOption -> GenInstanceConfig
allInstances ov = GenInstanceConfig ov GenOneClass True True True

-- | A default name munging scheme for use with GenClassPerType.  Munges special
-- characters into their ASCII (or is it UTF?) code determined by ord,
-- prefixed by two underscores.
--
-- Given a string with a type name, such as "Map Int (Maybe ([String],Bool))"
-- this function must munge it into a valid suffix for a Haskell identifier,
-- i.e. using only alphanumeric characters, apostrophe and underscore.
-- Also, there may be type-level operators such as "->".  I was going to let users
-- override this, but any user that creates types like Foo__32Bar gets what they
-- deserve.
mungeName :: String -> String
mungeName = concatMap munge
  where
    munge :: Char -> String
    munge x
      | isAlphaNum x = [x]
      | otherwise = "__" ++ show (ord x)

-- | A type that represents a generator for instances of a set of types.
newtype GenInstance = GenInstance (TypeMapM ())

-- | Generates instances for all types within the given type.  Therefore you should
-- only need one or two of these calls to cover all of a complex data structure,
-- by calling this on the largest types in your structure.  The function is non-strict
-- in its argument, so the easiest way to call it is:
--
-- > genInstance (undefined :: MyType)
genInstance :: Data t => t -> GenInstance
genInstance = GenInstance . findTypesIn (const Nothing)

data Witness
  = Plain { witness :: DataBox }
    | Detailed { witness :: DataBox
               , _directlyContains :: [DataBox]
               -- First is funcSameType, second is funcNewType:
               , _processChildrenMod :: ClassType -> (FuncType -> String, FuncType -> String) -> [String]
               }

-- The Eq instance is based on the inner type.
instance Eq Witness where
  (==) wx wy = case (witness wx, witness wy) of
    (DataBox x, DataBox y) -> typeRep (makeProxy x) == typeRep (makeProxy y)

makeProxy :: t -> Proxy t
makeProxy _ = Proxy

funcPlain :: FuncType -> String
funcPlain Func = ""
funcPlain FuncM = "return"
funcPlain FuncA = "pure"
funcPlain FuncMRoute = "return"
funcPlain FuncARoute = "pure"

funcAp :: FuncType -> String
funcAp Func = " "
funcAp FuncM = "`ap`"
funcAp FuncA = "<*>"
funcAp FuncMRoute = "`ap`"
funcAp FuncARoute = "<*>"

funcTraverse :: FuncType -> String
funcTraverse Func = "fmap"
funcTraverse FuncM = "T.mapM"
funcTraverse FuncA = "T.traverse"
funcTraverse FuncMRoute = "T.mapM"
funcTraverse FuncARoute = "T.traverse"

funcsForClass :: ClassType -> [FuncType]
funcsForClass ct = case ct of
      ClassAlloy -> [Func]
      ClassAlloyA -> [FuncA, FuncM]
      ClassAlloyARoute -> [FuncARoute, FuncMRoute]

-- | Generates an instance for the 'Data.Map.Map' type.  Map is a difficult type
-- because its instance of Data hides its implementation, so we can't actually
-- use the Data instance to work out what the children are (as we can do for other
-- algebraic data types).  So for every different Map that you want to process
-- (or that you have inside other types you want to process), you must also call
-- this function to effectively notify the generation-functions of the existence
-- of your map.  We wish there was an easier, non-hacky way but we can't see one.
genMapInstance :: forall k v. (Ord k, Data k, Data v) => k -> v -> GenInstance
genMapInstance k v
  = GenInstance $ do
       -- Must find types for contained types, in case they are not generated elsewhere.
       --  This is true for Tock, where NameDefs only exist in AST or CompState
       -- in a Map.
       findTypesIn (const Nothing) (k, v) -- This does types in k and v, and their pairing
       tk <- liftIO $ typeKey m
       modify (Map.insert tk (toQualName m,
         Detailed (DataBox m) [DataBox (k, v), DataBox k, DataBox v]
         (\cl (funcSameType, funcNewType) -> concat [
           case cl of
             ClassAlloyARoute ->
               [funcSameType b ++ " _ ops (v, r) = let mns = zip (Map.toList v) (map ((r @->) . routeDataMap) [0..]) in"
               ,"  " ++ funcPlain b ++ " Map.fromList " ++ funcAp b
                 ++ " (" ++ funcTraverse b ++ " (" ++ funcNewType b ++ " ops BaseOpARoute) mns)"
               ]
             _ -> let terminator = case cl of
                                     ClassAlloyA -> "BaseOpA"
                                     ClassAlloy -> "BaseOp" in
               [funcSameType b ++ " _ ops v = " ++ funcPlain b ++ " Map.fromList "
                 ++ funcAp b ++ " (" ++ funcTraverse b ++ " (" ++ funcNewType b
                   ++ " ops " ++ terminator ++ ") (Map.toList v))"
               ]
           | b <- funcsForClass cl])
         ))
  where
    m :: Map k v
    m = undefined

-- | Generates an instance for the 'Data.Set.Set' type.  See 'genMapInstance' for
-- an explanation.
genSetInstance :: forall a. (Ord a, Data a) => a -> GenInstance
genSetInstance x
  = GenInstance $ do
       -- Must find types for contained types, in case they are not generated elsewhere.
       findTypesIn (const Nothing) x
       tk <- liftIO $ typeKey s
       modify (Map.insert tk (toQualName s,
         Detailed (DataBox s) [DataBox x]
         (\cl (funcSameType, funcNewType) -> concat [
           case cl of
             ClassAlloyARoute ->
               [funcSameType b ++ " _ ops (v, r) = let sns = zip (Set.toList v) (map ((r @->) . routeDataSet) [0..]) in"
               ,"  " ++ funcPlain b ++ " Set.fromList " ++ funcAp b
                 ++ " (" ++ funcTraverse b ++ " (" ++ funcNewType b ++ " ops BaseOpARoute) sns)"
               ]
             _ -> let terminator = case cl of
                                     ClassAlloyA -> "BaseOpA"
                                     ClassAlloy -> "BaseOp" in
                [funcSameType b ++ " _ ops v = " ++ funcPlain b ++ " Set.fromList "
                 ++ funcAp b ++ " (" ++ funcTraverse b ++ " (" ++ funcNewType b
                   ++ " ops " ++ terminator ++ ") (Set.toList v))"]
           | b <- funcsForClass cl])

        ))
  where
    s :: Set a
    s = undefined

-- | Generates an instance for the 'Data.Vector.Vector' type. Like
-- 'Data.Map.Map', the Data instance for Vector hides its representation.
genVectorInstance :: forall v. (Data v) => v -> GenInstance
genVectorInstance v = GenInstance $ do
  findTypesIn (const Nothing) v
  tk <- liftIO $ typeKey m
  modify $ Map.insert tk $ (,) (toQualName m)
    $ Detailed (DataBox m) [DataBox v]
      $ \ cl (funcSameType, funcNewType) -> concat
        [ case cl of
          ClassAlloyARoute -> map concat
            [ [ funcSameType b
              , " _ ops (v, r) = let mns = zip (Vector.toList v) (map ((r @->) . routeDataMap) [0..]) in"
              ]
            , [ "  "
              , funcPlain b
              , " Vector.fromList "
              , funcAp b
              , " ("
              , funcTraverse b
              , " ("
              , funcNewType b
              , " ops BaseOpARoute) mns)"
              ]
            ]
          _ -> ((:[]) . concat)
            [ funcSameType b
            , " _ ops v = "
            , funcPlain b
            , " Vector.fromList "
            , funcAp b
            , " ("
            , funcTraverse b
            , " ("
            , funcNewType b
            , " ops "
            , terminator
            , ") (Vector.toList v))"
            ]
            where
              terminator = case cl of
                ClassAlloyA -> "BaseOpA"
                ClassAlloy -> "BaseOp"

        | b <- funcsForClass cl
        ]
  where
    m :: Vector v
    m = undefined
  
-- Explanation of Alloy's instances:
--
-- Alloy is a type-class system for automatically applying generic transformations
-- to the first instance of a specific type in a large data structure.
--
-- A set of operations is represented as a tuple list, e.g.
--
-- > (a -> m a, (b -> m b, (c -> m c, ())))
--
-- The unit type is the list terminator.
--
-- The Alloy class takes four parameters:
--
-- * The first is the type currently being processed.
--
-- * The second is the list of operations still to be checked against the current type.
--
-- * The third is the list of operations to be applied if we end up processing
-- the current type's children.
--
-- * The fourth is the monad in which it operates, which is just passed through.
--
-- There are broadly four types of instance generated by this module:
-- 
-- * The "exact match" instance.  These are of the form:
-- 
-- > instance Monad m => AlloyA a (a -> m a, r) ops m where
-- >   transformM (f,_) _ v = f v
-- 
-- This just applies the transformation directly, as you can see, ignoring the
-- other bits and bobs.
-- 
-- * The "process children" instance.  For a data type:
--
-- > data Foo = ConstrBar Bar | ConstrBazQuux Baz Quux
--
-- This is of the form:
-- 
-- > instance (Monad m,
-- >           AlloyA Bar (f,ops) () m,
-- >           AlloyA Baz (f,ops) () m,
-- >           AlloyA Quux (f,ops) () m) =>
-- >         AlloyA Foo () (f, ops) m where
-- >  transformM () ops (ConstrBar a0)
-- >    = do r0 <- transformM ops () a0
-- >         return (ConstrBar r0)
-- >  transformM () ops (ConstrBazQuux a0 a1)
-- >    = do r0 <- transformM ops () a0
-- >         r1 <- transformM ops () a1
-- >         return (ConstrBazQuux r0 r1)
--
-- The reason for using (f, ops) in the type-class header is to distinguish this
-- from the empty set of operations (see lower down).  The operations that are
-- to be applied on descent (the third parameter) are passed to the sub-instances
-- as the list of operations to be checked (the second parameter), with a new blank
-- list of operations to apply on descent.  The bodies of the methods just apply
-- transformM to each child of the constructor, and pull the data-type back together
-- again.
--
--
-- * The "can contain" instance.  This is of the form:
--
-- > instance (Monad m, AlloyA t r (a -> m a, ops) m) =>
-- >         AlloyA t (a -> m a, r) ops m where
-- >  transformM (f, rest) ops v = transformM rest (f, ops) v
--
-- Here, the type being considered, t, /can/ contain the type referred to by the
-- operation, a.  So we transfer the operation from the list we're processing onto
-- the list to apply in case of direct recursion.  Then we continue processing
-- the list of operations.
--
-- * The "cannot contain" instance.  This is of the form:
--
-- > instance (Monad m, AlloyA t r ops m) =>
-- >         AlloyA t (a -> m a, r) ops m where
-- >  transformM (_, rest) ops v = transformM rest ops v
--
-- This instance is based on the logic that if we have worked out that a big type
-- (like Foo) cannot contain a certain type (say, String) then by implication,
-- neither of its children can contain Strings either.  So we omit the transformation
-- of the type (in this example String) when we directly descend into Foo, by not
-- copying the transformation onto the third parameter.
--
-- The final thing we need, is a base case
-- for when both the second and third parameters are empty.  This means there are
-- no operations left to examine, but also none available for direct recursion.
-- At this point we just return the value unchanged.

data ClassType = ClassAlloy | ClassAlloyA | ClassAlloyARoute deriving (Eq)

instance Show ClassType where
  show ClassAlloy = "Alloy"
  show ClassAlloyA = "AlloyA"
  show ClassAlloyARoute = "AlloyARoute"

data FuncType = Func | FuncA | FuncM | FuncMRoute | FuncARoute deriving (Eq)

instance Show FuncType where
  show Func = "transform"
  show FuncA = "transformA"
  show FuncM = "transformM"
  show FuncARoute = "transformARoute"
  show FuncMRoute = "transformMRoute"

-- | Instances for a particular data type (i.e. where that data type is the
-- first argument to 'Alloy').
instancesFrom :: forall t. Data t => GenOverlappedOption -> GenClassOption ->
  ClassType -> [Witness] -> t -> IO [String]
instancesFrom genOverlapped genClass genClassType boxes w
    = do (specialProcessChildren, containedTypes) <-
           case find (== Plain (DataBox w)) boxes of
             Just (Detailed _ containedTypes doChildren) ->
               -- It's a special case, use the detailed info:
               do eachContained <- sequence [findTypesIn' useBoxes c | DataBox c <- containedTypes]
                  return (Just (containedTypes, doChildren), foldl Map.union Map.empty eachContained)
             -- It's a normal case, use findTypesIn' directly:
             _ -> do ts <- findTypesIn' useBoxes w
                     return (Nothing, ts)
         containedKeys <- liftM Set.fromList
           (sequence [typeKey c | DataBox c <- map witness $ justBoxes containedTypes])
         wKey <- typeKey w
         otherInsts <- sequence [do ck <- typeKey c
                                    return (otherInst wKey containedKeys c ck)
                                | DataBox c <- map witness boxes]
         return $ baseInst specialProcessChildren ++ concat otherInsts
  where
    useBoxes k = do b <- lookup k (zip (map witness boxes) boxes)
                    case b of
                      Plain {} -> Nothing
                      Detailed _ contains _ -> Just contains
    
    (wName, wMod) = toQualNameMod w
    wMunged = mungeName wName
    wDType = dataTypeOf w
    wCtrs = if isAlgType wDType then dataTypeConstrs wDType else []

    ctrArgs ctr
        = gmapQ DataBox (fromConstr ctr :: t)
    ctrArgTypes types
        = [toQualName w | DataBox w <- types]

    -- Given the context (a list of instance requirements), the left-hand ops,
    -- the right-hand ops, and a list of lines for the body of the class, generates
    -- an instance.
    --
    -- For GenOneClass this will be an instance of AlloyA.
    --
    -- For GenClassPerType this will be an instance of AlloyAFoo (or whatever)
    --
    -- For GenSlowDelegate this will be an instance of AlloyA', with the first
    -- and last arguments swapped.
    genInst :: [String] -> String -> String -> [String] -> [String]
    genInst context ops0 ops1 body
      = ["instance (" ++ concat (intersperse ", " context) ++ ") =>"
        ,"         " ++ contextSameType ops0 ops1 ++ " where"
        ] ++ map ("  " ++) body

    -- Generates the name of an instance for the same type with the given two ops
    -- sets.  The class name will be the same as genInst.
    contextSameType :: String -> String -> String
    contextSameType ops0 ops1 = show genClassType ++ case genClass of
      GenOneClass -> " (" ++ wName ++ ") " ++ ops0 ++ " " ++ ops1
      GenClassPerType -> wMunged ++" " ++ ops0 ++ " " ++ ops1
      GenSlowDelegate -> "' " ++ ops0 ++ " " ++ ops1 ++ " (" ++ wName ++ ")"

    -- Generates the name of an instance for a different type (for processing children).
    --  This will be AlloyA or AlloyA'.
    contextNewType :: String -> String -> String -> String
    contextNewType cName ops0 ops1 = show genClassType ++ case genClass of
      GenOneClass -> " (" ++ cName ++ ") " ++ ops0 ++ " " ++ ops1
      GenClassPerType -> " (" ++ cName ++ ") " ++ ops0 ++ " " ++ ops1
      GenSlowDelegate -> "' " ++ ops0 ++ " " ++ ops1 ++ " (" ++ cName ++ ")"
      

    -- The function to define in the body, and also to use for processing the same
    -- type.
    funcSameType :: FuncType -> String
    funcSameType func = case genClass of
      GenClassPerType -> base ++ wMunged
      GenOneClass -> base
      GenSlowDelegate -> base ++ "'"
      where
        base = show func

    -- The function to use for processing other types
    funcNewType :: FuncType -> String
    funcNewType func = case genClass of
      GenClassPerType -> base
      GenOneClass -> base
      GenSlowDelegate -> base ++ "'"
      where
        base = show func

    terminator :: String
    terminator = case genClassType of
      ClassAlloy -> "BaseOp"
      ClassAlloyA -> "BaseOpA"
      ClassAlloyARoute -> "BaseOpARoute"

    cons :: String
    cons = case genClassType of
      ClassAlloy -> ":-"
      ClassAlloyA -> ":-*"
      ClassAlloyARoute -> ":-@"

    funcs :: [FuncType]
    funcs = funcsForClass genClassType

    justData :: String
    justData = case genClassType of
      ClassAlloyARoute -> "(v, _)"
      _ -> "v"

    hasRoute = genClassType == ClassAlloyARoute

    -- | An instance that describes what to do when we have no transformations
    -- left to apply.  You can pass it an override for the case of processing children
    -- (and the types that make up the children).
    baseInst :: Maybe ([DataBox], ClassType -> (FuncType -> String, FuncType -> String) -> [String]) -> [String]
    baseInst mdoChildren
        = concat
          [genInst context terminator ("(f " ++ cons ++ " ops)") $
              maybe
                (concat
                [if isAlgType wDType
                    -- An algebraic type: apply to each child if we're following.
                    then (concatMap (constrCase b) wCtrs)
                    -- A primitive (or non-represented) type: just return it.
                    else [funcSameType b ++ " _ _ " ++ justData ++ " = " ++ funcPlain b ++ " v"]
                | b <- funcs])
                (\(_,f) -> f genClassType (funcSameType, funcNewType)) mdoChildren
          ,genInst [] terminator terminator
             [funcSameType b ++ " _ _ " ++ justData ++ " = " ++ funcPlain b ++ " v" | b <- funcs]
          ,if genOverlapped == GenWithoutOverlapped then [] else
            genInst
              [ contextSameType "r" "ops" ]
              ("(a " ++ cons ++ " r)") "ops" 
                [funcSameType b ++ " (_ " ++ cons ++ " rest) ops vr = " ++ funcSameType b ++ " rest ops vr"
                | b <- funcs]
          ,if genClass == GenClassPerType
             then error "GenClassPerType currently unsupported" {-["class AlloyARoute" ++ wMunged ++ " o o' where"]
                  ++ concat [
                  ,"  " ++ funcSameType b ++ " :: Monad m => o m outer -> o' m outer -> (" ++ wName
                    ++ ", Route (" ++ wName ++ ") outer) -> m (" ++ wName ++ ")"
                  ,"  " ++ funcSameType b ++ " :: Applicative a => o a outer -> o' a outer -> (" ++ wName
                    ++ ", Route (" ++ wName ++ ") outer) -> a (" ++ wName ++ ")"
                  | b <- funcs]
                  ,""
                  ,"instance (" ++ contextSameType "o0" "o1" ++ ") =>"
                  ,"         AlloyARoute (" ++ wName ++ ") o0 o1 where"
                  ,"  transformMRoute = " ++ funcSameType True
                  ,"  transformARoute = " ++ funcSameType False
                  ] -}
             else []
          ]
      where
        -- | Class context for 'baseInst'.
        -- We need an instance of Alloy for each of the types directly contained within
        -- this type, so we can recurse into them.
        context :: [String]
        context
          = [ contextNewType argType ("(f " ++ cons ++ " ops)") terminator
            | argType <- nub $ sort $ concatMap ctrArgTypes $
                maybe (map ctrArgs wCtrs) ((:[]) . fst) mdoChildren]

    -- | A 'transformM' case for a particular constructor of this (algebraic)
    -- data type: pull the value apart, apply 'transformM' to each part of it,
    -- then stick it back together.
    constrCase :: FuncType -> Constr -> [String]
    constrCase b ctr
        = [ funcSameType b ++ " _ " ++ (if argNums == [] then "_" else "ops") ++
            " (" ++ ctrInput ++ (if hasRoute then " , " ++ (if argNums == [] then "_" else "rt") else "") ++ ")"
          , "    = " ++ funcPlain b ++ " " ++ ctrName
          ] ++
          [ " " ++ funcAp b ++ " (" ++ funcNewType b ++ " ops " ++ terminator ++ " (a" ++ show i
                        ++ (if hasRoute then ", rt @-> makeRoute [" ++ show i ++ "] "
                        ++ "(\\f (" ++ ctrMod ++ ") -> f b" ++ show i
                        ++ " >>= (\\b" ++ show i ++ " -> return (" ++ ctrMod ++ ")))"
                          else "") ++ "))"
           | i <- argNums]
      where
        argNums = [0 .. ((length $ ctrArgs ctr) - 1)]
        ctrS = show ctr
        ctrName = wMod ++ ctrS
        makeCtr vs = ctrName ++ concatMap (" " ++) vs
        ctrInput = makeCtr ["a" ++ show i | i <- argNums]
        ctrMod = makeCtr ["b" ++ show i | i <- argNums]

    -- | An instance that describes how to apply -- or not apply -- a
    -- transformation.
    otherInst :: Data s => TypeRep -> Set.Set TypeRep -> s -> TypeRep -> [String]
    otherInst wKey containedKeys c cKey
        = if not shouldGen then [] else
            genInst context
                    ("((" ++ cName ++ ") " ++ cons ++ " r)")
                    "ops"
                    impl
      where
        cName = toQualName c
        (shouldGen, context, impl)
          -- This type matches the transformation: apply it.
          | wKey == cKey
            = (True
              ,[]
              ,[funcSameType b ++ " (f " ++ cons ++ " _) _ vr = f vr" | b <- funcs])
          -- This type might contain the type that the transformation acts
          -- upon
          | cKey `Set.member` containedKeys
            = (True
              ,[contextSameType "r" ("((" ++ cName ++ ") " ++ cons ++ " ops)")]
              ,[funcSameType b ++ " (f " ++ cons ++ " rest) ops vr = " ++ funcSameType b ++ " rest (f " ++ cons ++ " ops) vr"
               | b <- funcs])
          -- This type can't contain the transformed type; just move on to the
          -- next transformation.
          | genOverlapped == GenWithoutOverlapped
            = (True
              ,[contextSameType "r" "ops"]
              ,[funcSameType b ++ " (_ " ++ cons ++ " rest) ops vr = " ++ funcSameType b ++ " rest ops vr"
               | b <- funcs])
          -- This is covered by one big overlapping instance:
          | otherwise = (False,[],[])

-- | The lines in the header that form the import statements necessary for the
-- Alloy instances.
instanceImports :: [String]
instanceImports = map ("import " ++) ["Control.Applicative", "Control.Monad", "Data.Generics.Alloy", "qualified GHC.Types"]

-- | Like 'instanceImports' but also adds the lines needed for maps and sets.
-- If you use 'genMapInstance' or 'genSetInstance', use this function, otherwise
-- 'instanceImports' will suffice.
instanceImportsMapSet :: [String]
instanceImportsMapSet = instanceImports ++
  map ("import " ++) ["Data.Map(Map)", "qualified Data.Map as Map"
                     ,"Data.Set(Set)", "qualified Data.Set as Set"
                     ,"qualified Data.Traversable as T"
                     ]

-- | Like 'instanceImportsMapSet' but for 'Data.Vector.Vector'.
instanceImportsVector :: [String]
instanceImportsVector = instanceImports ++ map ("import " ++)
  [ "Data.Vector (Vector)"
  , "qualified Data.Vector as Vector"
  , "qualified Data.Traversable as T"
  ]

-- | Generates all the given instances (eliminating any duplicates)
-- with the given options.  The return is a list of lines of code.  This should
-- then be written to a Haskell module with the appropriate header.
genInstances :: GenInstanceConfig -> [GenInstance] -> IO [String]
genInstances opts insts
  =  do typeMap <- flip execStateT Map.empty (sequence [g | GenInstance g <- insts])
        let inst = [instancesFrom
                      (genOverlapped opts)
                      (genClass opts)
                      classType
                      (justBoxes typeMap)
                      w
                   | DataBox w <- map witness $ justBoxes typeMap,
                     classType <- classTypes]
        inst' <- sequence inst
        return $ concat inst'
  where
    classTypes = concat
      [ [ClassAlloy | genPure opts]
      , [ClassAlloyA | genEffect opts]
      , [ClassAlloyARoute | genRoute opts]
      ]

-- | The line with a LANGUAGE pragma detailed all the extensions needed.  This
-- is automatically written by 'writeInstances' and 'writeInstancesTo' at the top
-- of the file, but you may want to use it if you are using 'genInstances'.
languageExtras :: GenOverlappedOption -> String
languageExtras opt = "{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances"
  ++ if opt == GenWithOverlapped
       then ",OverlappingInstances #-}"
       else "#-}"

-- | Generates the instances according to the options and writes it to stdout with
-- the given header (the header is a list of lines without newline characters).
--
-- The configuration can be obtained from 'justPure' (for example) or constructing
-- the configuration yourself.  The list of 'GenInstance' can be obtained through
-- 'genInstance'.  The header will generally be something like:
-- 
-- > "module FooInstances where" : "import qualified Foo" : instanceImports
writeInstances :: GenInstanceConfig -> [GenInstance] -> [String] -> IO ()
writeInstances opts inst header
  = do instLines <- genInstances opts inst
       putStr (unlines (languageExtras (genOverlapped opts) : (header ++ instLines)))

-- | Generates the instances according to the options and writes it to the given filename with
-- the given header (the header is a list of lines without newline characters).
writeInstancesTo :: GenInstanceConfig -> [GenInstance] -> [String]
  -> FilePath -> IO ()
writeInstancesTo opts inst header fileName
  = do instLines <- genInstances opts inst
       writeFile fileName (unlines (languageExtras (genOverlapped opts) : (header ++ instLines)))


--{{{ Various SYB-based functions that we don't export, for discovering contained types:

-- | A type that can contain any 'Data' item.
data DataBox = forall t. Data t => DataBox t

instance Eq DataBox where
  (==) (DataBox x) (DataBox y) = typeRep (makeProxy x) == typeRep (makeProxy y)

type TypeMap = Map TypeRep (String, Witness)
type TypeMapM = StateT TypeMap IO

typeKey :: Typeable t => t -> IO TypeRep
typeKey x = return $ typeRep (makeProxy x)

toQualName :: Typeable t => t -> String
toQualName w = fst (toQualNameMod w)

-- First item is name of type, with everything inside qualified
-- Second item is the module prefix (if any) of the outermost type
toQualNameMod :: Typeable t => t -> (String, String)
toQualNameMod w = toQualNameMod' (typeRep (makeProxy w))

toQualNameMod' :: TypeRep -> (String, String)
toQualNameMod' tr = case (show con, args) of
  ("[]", [arg]) -> -- Lists are special case, don't qualify and use [ ] to wrap:
                ("[" ++ fst (toQualNameMod' arg) ++ "]", "")
  ("()", []) -> ("()", "") -- Unit is special, don't qualify
  ('(':',':_, args) -> -- Tuples are also special, don't qualify:
       ("(" ++ show con ++ concat (Prelude.map ((' ' :) . fst . toQualNameMod') args) ++ ")"
       , "")    
      -- Special case Integer, whose definition is hidden:
  _ | qualCon con == "GHC.Integer.Type.Integer" -> ("Prelude.Integer", "")
  _ -> ("(" ++ qualCon con ++ concat (Prelude.map ((' ' :) . fst . toQualNameMod') args) ++ ")"
       , modPrefix con)
  where
    (con, args) = splitTyConApp tr

qualCon :: TyCon -> String
qualCon wc = modPrefix wc ++ show wc

modPrefix :: TyCon -> String
modPrefix wc = case Ty.tyConModule wc of
                 "" -> ""
                 m -> m ++ "."

findTypesIn' :: Data t => (DataBox -> Maybe [DataBox]) -> t -> IO TypeMap
findTypesIn' f x = execStateT (findTypesIn f x) Map.empty

-- | Given a starting value, find all the types that could possibly be inside
-- it.
findTypesIn :: Data t => (DataBox -> Maybe [DataBox]) -> t -> TypeMapM ()
findTypesIn custom start = doType start
  where
    doType :: Data t => t -> TypeMapM ()
    doType x
        =  do map <- get
              key <- liftIO $ return rep
              when (not $ key `Map.member` map) $
                 do modify $ Map.insert key (reps, Plain (DataBox x))
                    case custom $ DataBox x of
                      Just inside -> sequence_ [doType y | DataBox y <- inside]
                      Nothing ->                  
                        when (isAlgType dtype) $
                          mapM_ doConstr $ dataTypeConstrs dtype
      where
        rep = typeRep (makeProxy x)        
        reps = show rep
        dtype = dataTypeOf x

        doConstr :: Constr -> TypeMapM ()
        doConstr ctr
            = sequence_ [doType x' | DataBox x' <- args]
          where
            args = gmapQ DataBox (asTypeOf (fromConstr ctr) x)

-- | Reduce a 'TypeMap' to a list of 'Witness'es, sorted by name.
justBoxes :: TypeMap -> [Witness]
justBoxes = map snd . sortBy (comparing fst) . Map.elems

--}}}
