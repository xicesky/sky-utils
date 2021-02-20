
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}

-- Type magic
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DefaultSignatures      #-}

{-|
Module      : Container.Monoidal
Description : Containers that can be constructed from elements
Stability   : experimental

-}

module Container.MonoidalC
    (   MonoidalC(..)
    ) where

import Prelude
    (   Eq(..), Ord(..), Int, Bool(..), Show(..)
    ,   ($), (.)
    ,   id, const
    )

import Data.Monoid
import Control.Applicative (Applicative(..))

import Container.ITraversable

-- These imports are for defining instances
import Container.Containers

import           Data.Hashable
import qualified Data.List              as List
import qualified Data.Map               as TreeMap
import qualified Data.Set               as TreeSet
import qualified Data.HashMap.Lazy      as HashMap
import qualified Data.HashMap.Strict    as StrictHashMap
import qualified Data.HashSet           as HashSet

{-| Monoidal constructible container.

    These are arbitrary-size containers (including zero size), that
    can - for example - be constructed from a list of elements.
-}
class (Monoid c, ITraversable c) => MonoidalC c where
    -- | Empty container
    cempty :: c
    cempty = mempty

    {-| Combination of two existing containers

        This can change existing keys!
        This can be destructive - especially if the container doesn't change keys!
        I am not happy with this function (it makes very few guarantees!)

        Actual requirements:
            -- Stability on values
            omap f (a `ccombine` b) = (omap f a) `ccombine` (omap f b)

        If your container is actually an @Applicative f@ over a type @a@:
            pure = csingleton

        Can't guarantee - only for "list-like" containers:
            -- Only if list-like
            foldMapIndexed f (a `ccombine` b) =
                foldMapIndexed f a <> foldMapIndexed f b
            -- Only if list-like
            size (a `ccombine` b) = size a + size b

        Can't guarantee - only for "map-like" containers:
            -- Preservation of keys
            forall k: either
                lookup k (a `ccombine` b) = lookup k a
            or
                lookup k (a `ccombine` b) = lookup k b

            (lookup k a = Nothing) => (lookup k (a `ccombine` b)) = lookup k b)
            -- and similarly for lookup k b = Nothing
            -- is this enough to derive the rules for omap?

            -- guaranteed left bias?
            (lookup k a = Just v) => (lookup k (a `ccombine` b)) = Just v)

    -}
    ccombine :: c -> c -> c
    ccombine = (<>)

    cinsert :: ElemT c -> c -> c
    cinsert e c = csingleton e `ccombine` c

    -- | Container with exactly one element
    csingleton :: ElemT c -> c
    csingleton e = cinsert e cempty

    {- MINIMAL cempty, ccombine, (csingleton | cinsert) -}
    {-# MINIMAL csingleton | cinsert #-}

    -- * Derived functions
    -- | You might want to override some of them for increased performace

    -- TODO: generalize
    -- TODO: guarantee: fromList . listElements = id
    fromList :: [ElemT c] -> c
    fromList [] = cempty
    fromList (x:xs) = cinsert x (fromList xs)

{-  Further candidates:
    filter :: (k -> v -> Bool) -> c -> c
-}

----------------------------------------------------------------------------------------------------

-- * Instances of MonoidalC

instance MonoidalC [a] where
    csingleton = pure
    cinsert = (:)
    fromList = id

instance Ord a => MonoidalC (TreeSet a) where
    csingleton = TreeSet.singleton
    cinsert = TreeSet.insert
    fromList = TreeSet.fromList

instance (Eq a, Hashable a) => MonoidalC (HashSet a) where
    csingleton = HashSet.singleton
    cinsert = HashSet.insert
    fromList = HashSet.fromList

instance Ord k => MonoidalC (TreeMap k a) where
    csingleton (k, v) = TreeMap.singleton k v
    cinsert (k, v) = TreeMap.insert k v
    fromList = TreeMap.fromList

instance (Eq k, Hashable k) => MonoidalC (HashMap k a) where
    csingleton (k, v) = HashMap.singleton k v
    cinsert (k, v) = HashMap.insert k v
    fromList = HashMap.fromList
