
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
Module      : Container.MapLike
Description : Map-Like containers that contain arbitrary key-value pairs.
Stability   : experimental

-}

module Container.MapLike
    (   MapLike(..)
    ) where

import Prelude
    (   Eq(..), Ord(..), Int, Bool(..), Show(..)
    ,   ($), (.)
    ,   id, const
    ,   fromIntegral, otherwise, flip
    )

import Data.Functor (Functor(..))
import Data.Functor.Identity (Identity(..))
import Control.Applicative (Applicative(..))
import Data.Monoid
import Data.Maybe (Maybe(..))

import Container.ITraversable
import Container.MonoidalC

-- These imports are for defining instances
import Container.Containers

import           Data.Hashable
import qualified Data.Map               as TreeMap
import qualified Data.HashMap.Lazy      as HashMap
import qualified Data.HashMap.Strict    as StrictHashMap

{-| Map-Like constructible container.

    Values can be inserted at arbitrary keys.
-}
class (MonoidalC c, (IndexT c, ValueT c) ~ ElemT c) => MapLike c where
    {- TODO: This _could_ be used more generally for containers with
        arbitrary indices (and be a superclass of MapLike and SetLike!)

        We also need something stronger than alterF for key changes.
    -}
    alterF :: (Functor f) =>
        (Maybe (ValueT c) -> f (Maybe (ValueT c))) -> IndexT c -> c -> f c
    
    {-# MINIMAL alterF #-}

    {- TODO: With the guarantee (IndexT c, ValueT c) ~ ElemT c
        and MonoidalC, we can already implement some functions
        by rebuilding the container. Is it enough to implement alterF?
    -}

    -- * Derived functions
    -- | You might want to override some of them for increased performace

    alter :: (Maybe (ValueT c) -> Maybe (ValueT c)) -> IndexT c -> c -> c
    alter f = (runIdentity .) . alterF (Identity . f)

    mapInsert :: IndexT c -> ValueT c -> c -> c
    mapInsert k v = alter (const (Just v)) k

    mapDelete :: IndexT c -> c -> c
    mapDelete = alter (const Nothing)

    keys :: (Applicative m, Monoid (m (IndexT c))) => c -> m (IndexT c)
    keys = listIndices

    hasKey :: c -> IndexT c -> Bool
    hasKey = flip hasIndex

instance Ord k => MapLike (TreeMap k a) where
    alterF = TreeMap.alterF
    alter = TreeMap.alter
    mapInsert = TreeMap.insert
    mapDelete = TreeMap.delete

instance (Eq k, Hashable k) => MapLike (HashMap k a) where
    alterF = HashMap.alterF
    alter = HashMap.alter
    mapInsert = HashMap.insert
    mapDelete = HashMap.delete
