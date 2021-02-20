
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
Module      : Container.SetLike
Description : Set-like unordered containers.
Stability   : experimental

-}

module Container.SetLike
    (   SetLike(..)
    ) where

import Prelude
    (   Eq(..), Ord(..), Int, Bool(..), Show(..)
    ,   ($), (.)
    ,   id, const
    ,   fromIntegral, otherwise, flip
    )

import Data.Functor (Functor(..), (<$>))
import Data.Functor.Identity (Identity(..))
import Control.Applicative (Applicative(..))
import Data.Monoid
import Data.Maybe (Maybe(..))
import Data.Proxy (Proxy(..))

import Container.ITraversable
import Container.MonoidalC

-- These imports are for defining instances
import Container.Containers

import           Data.Hashable
import qualified Data.Set       as TreeSet
import qualified Data.HashSet   as HashSet


{-| Set-Like constructible container.

    Contains elements at most once.
    @ValueT@ is of minor importance, as values are actually equal to keys.
-}
class (MonoidalC c, IndexT c ~ ElemT c) => SetLike c where
    {- TODO: This _could_ be used more generally for containers with
        arbitrary indices (and be a superclass of MapLike and SetLike!)
        But we need ValueT ~ () so it's actually isomorphic for sets.

        We also need something stronger than alterF for key changes.
    -}
    alterSetF :: (Functor f) =>
        (Bool -> f Bool) -> IndexT c -> c -> f c

    -- | Proof that values are derived from keys
    deriveValue :: Proxy c -> IndexT c -> ValueT c
    -- default deriveValue :: (ValueT c ~ ()) => Proxy c -> IndexT c -> ValueT c
    -- deriveValue _ _ = ()

    {-# MINIMAL alterSetF, deriveValue #-}

    -- * Derived functions
    -- | You might want to override some of them for increased performace

    alterSet :: (Bool -> Bool) -> IndexT c -> c -> c
    alterSet f = (runIdentity .) . alterSetF (Identity . f)

    setInsert :: IndexT c -> c -> c
    setInsert = alterSet (const True)

    setDelete :: IndexT c -> c -> c
    setDelete = alterSet (const False)

    elements :: (Applicative m, Monoid (m (IndexT c))) => c -> m (IndexT c)
    elements = listIndices

    member :: IndexT c -> c -> Bool
    member = hasIndex

instance Ord a => SetLike (TreeSet a) where
    -- FIXME stackage doesn't currently provide Data.Map with alterF
    -- latest is containers-0.6.2.1
    -- so we have to cheat a little here
    -- alterSetF = TreeSet.alterF

    alterSetF f a c = setIndex <$> f (a `member` c) where
        setIndex True = TreeSet.insert a c
        setIndex False = TreeSet.delete a c

    deriveValue _ = id

    setInsert = TreeSet.insert
    setDelete = TreeSet.delete
    member = TreeSet.member

instance (Eq a, Hashable a) => SetLike (HashSet a) where
    -- FIXME stackage doesn't currently provide Data.Map with alterF
    -- latest is containers-0.6.2.1
    -- so we have to cheat a little here
    -- alterSetF = HashSet.alterF

    alterSetF f a c = setIndex <$> f (a `member` c) where
        setIndex True = HashSet.insert a c
        setIndex False = HashSet.delete a c

    deriveValue _ = id

    setInsert = HashSet.insert
    setDelete = HashSet.delete
    member = HashSet.member
