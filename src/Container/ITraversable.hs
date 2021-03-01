
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
Module      : Container.ITraversable
Description : Indexed monomorphic traversables
Stability   : experimental

We'd like to view all containers as \"indexed\". Indexed (sometimes
called \"associative\") containers have values at certain \"positions\"
which we call indices. Each index is mapped to at most one value.

Indices do not imply order. Unordered containers can still have indices,
but they may just be the values themselves. (Or, for example in the case
@HashMap k ()@ the indices themselves are what the container \"contains\".)

This module defines @ITraversable@, the class of indexed traversable types
(containers) and provides instances for common containers.

-}

module Container.ITraversable
    (   ElemT, IndexT, ValueT
    ,   ITraversable(..)
    ) where

import Data.Kind (Type)

import Prelude
    (   Eq(..), Ord(..), Int, Bool(..), Show(..)
    ,   ($), (.)
    ,   id, const
    ,   fromIntegral, otherwise
    )

import Data.Functor (Functor(..), (<$>))
import Control.Applicative (Applicative(..), (<*>), Const(..))
import Data.Monoid
import Data.Traversable (Traversable(..))
import Data.Foldable (Foldable(..))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Functor.Const (Const(..))
import Data.Proxy (Proxy(..))
import Numeric.Natural (Natural)

-- These imports are for defining instances
import Container.Containers

import           Data.Hashable
import qualified Data.List              as List
import qualified Data.Map               as TreeMap
import qualified Data.Set               as TreeSet
import qualified Data.HashMap.Lazy      as HashMap
import qualified Data.HashMap.Strict    as StrictHashMap
import qualified Data.HashSet           as HashSet

-- * Containers have 3 types associated with them:

{-| The element type.

    This is not neccesarily the \"value type\".
-}
type family ElemT c :: Type

-- FIXME doc
type family IndexT c :: Type

-- FIXME doc
type family ValueT c :: Type

{-| Monomorphic, indexed traversable.

    This is a container that we can read and even
    deconstruct, but there is no defined way to construct it.

    This class is very similar to @MonoTraversable@ from the package
    "mono-traversable", but includes indices.
-}
class ITraversable c where

    -- * Essential functions

    -- | A version of @traverse@ for indexed, monomorphic types.
    traverseIndexed :: Applicative g =>
        (IndexT c -> ValueT c -> g (ValueT c)) -> c -> g c

    -- | Proof that elements are made of indices + values
    -- FIXME necessary?
    mkElement :: Proxy c -> IndexT c -> ValueT c -> ElemT c

    -- | Proof that values can be derived from elements
    -- FIXME necessary?
    unElement :: Proxy c -> ElemT c -> ValueT c

    {-# MINIMAL traverseIndexed, mkElement, unElement #-}

    -- * Derived functions
    -- You might want to override some of them for increased performace

    -- | Monomorphic traversal without indices
    otraverse :: Applicative f => (ValueT c -> f (ValueT c)) -> c -> f c
    otraverse = traverseIndexed . const

    -- | A monomorphic version of @foldMap@ with indices
    foldMapIndexed :: forall a m. (Monoid m) => (IndexT c -> ValueT c -> m) -> c -> m
    foldMapIndexed f = getConst . traverseIndexed f' where
        f' :: IndexT c -> ValueT c -> Const m (ValueT c)
        f' = (Const .) . f

    -- | A monomorphic version of @foldMap@
    ofoldMap :: forall m. Monoid m => (ValueT c -> m) -> c -> m
    ofoldMap = foldMapIndexed . const

    -- | A monomorphic version of @fmap@
    omap :: (ValueT c -> ValueT c) -> c -> c
    omap f = runIdentity . traverseIndexed (const $ Identity . f)

    -- | Look up a value at a specific index.
    lookup :: IndexT c -> c -> Maybe (ValueT c)
    default lookup :: Eq (IndexT c) => IndexT c -> c -> Maybe (ValueT c)
    lookup i = getFirst . foldMapIndexed match where
        match :: IndexT c -> ValueT c -> First (ValueT c)
        match j v | i == j  = First (Just v)
        match _ _           = mempty

    -- | Check if a value exists at a given index
    hasIndex :: IndexT c -> c -> Bool
    hasIndex = (isJust .) . lookup

    -- | Returns the number of elements in a container
    size :: c -> Natural
    size = getSum . (ofoldMap . const) (Sum 1)

    -- | Returns @true@ iff the container is empty
    isEmpty :: c -> Bool
    isEmpty = getAny . (ofoldMap . const) (Any True)

    -- | @findValue p c@ returns @true@ iff @p v@ is true for any value in c
    findValue :: (ValueT c -> Bool) -> c -> Bool
    findValue p = getAny . ofoldMap (Any . p)

    -- | Collect all elements in a @Applicative@ @Monoid@, e.g. List
    listElements :: forall m. (Applicative m, Monoid (m (ElemT c))) => c -> m (ElemT c)
    listElements = foldMapIndexed f' where
        f' :: IndexT c -> ValueT c -> m (ElemT c)
        f' k v = pure $ mkElement (Proxy :: Proxy c) k v

    -- | Collect all values in a @Applicative@ @Monoid@, e.g. List
    --      The resulting list might contain duplicates!
    listValues :: (Applicative m, Monoid (m (ValueT c))) => c -> m (ValueT c)
    listValues = ofoldMap pure

    listIndices :: forall m. (Applicative m, Monoid (m (IndexT c))) => c -> m (IndexT c)
    listIndices = foldMapIndexed (const . pure)

{-  Further candidates:
    mapWithKey :: (k -> va -> vb) -> m k va -> m k vb   -- but make it mono
-}

----------------------------------------------------------------------------------------------------

-- * Instances of ITraversable

-- ** Lists

type instance ElemT  [a] = a
type instance IndexT [a] = Int      -- This should really be "Word" or "Natural"!
type instance ValueT [a] = a

instance ITraversable [a] where
    
    traverseIndexed :: forall g. Applicative g => (Int -> a -> g a) -> [a] -> g [a]
    traverseIndexed g = go 0 where
        go :: Int -> [a] -> g [a]
        go i []     = pure []
        go i (x:xs) = (:) <$> g i x <*> traverseIndexed g xs

    mkElement :: Proxy [a] -> Int -> a -> a
    mkElement _ _ v = v

    unElement :: Proxy [a] -> a -> a
    unElement _ = id

    -- Overrides, the native impl may be faster

    otraverse = traverse
    ofoldMap = foldMap
    omap = fmap
    lookup i l = if i >= List.length l
        then Nothing
        else Just $ (List.!!) l i

-- ** TreeSet

type instance ElemT  (TreeSet a) = a
type instance IndexT (TreeSet a) = a
type instance ValueT (TreeSet a) = a   -- ?

instance Ord a => ITraversable (TreeSet a) where

    traverseIndexed :: forall g. Applicative g => (a -> a -> g a) -> TreeSet a -> g (TreeSet a)
    traverseIndexed g s =
        -- FIXME: I absolutely hate this.
        -- but Data.Set doesn't provide any version of traverse, so this works at first
        TreeSet.fromList <$> traverse f' (TreeSet.toList s)
        where
            f' :: a -> g a
            f' v = g v v

    mkElement :: Proxy (TreeSet a) -> a -> a -> a
    mkElement _ k _ = k

    unElement :: Proxy (TreeSet a) -> a -> a
    unElement _ = id

    -- Overrides, the native impl may be faster

    ofoldMap = foldMap  -- TreeSet implements Data.Foldable
    omap = TreeSet.map
    lookup :: a -> TreeSet a -> Maybe a
    lookup k c
        | TreeSet.member k c    = Just k
        | otherwise             = Nothing

-- ** HashSet

type instance ElemT  (HashSet a) = a
type instance IndexT (HashSet a) = a
type instance ValueT (HashSet a) = a   -- ?

instance (Eq a, Hashable a) => ITraversable (HashSet a) where

    traverseIndexed :: forall g. Applicative g => (a -> a -> g a) -> HashSet a -> g (HashSet a)
    traverseIndexed g s =
        -- FIXME: I absolutely hate this.
        -- but Data.Set doesn't provide any version of traverse, so this works at first
        HashSet.fromList <$> traverse f' (HashSet.toList s)
        where
            f' :: a -> g a
            f' v = g v v

    mkElement :: Proxy (HashSet a) -> a -> a -> a
    mkElement _ k _ = k

    unElement :: Proxy (HashSet a) -> a -> a
    unElement _ = id

    -- Overrides, the native impl may be faster

    ofoldMap = foldMap  -- HashSet implements Data.Foldable
    omap = HashSet.map
    lookup :: a -> HashSet a -> Maybe a
    lookup k c
        | HashSet.member k c    = Just k
        | otherwise             = Nothing

-- ** TreeMap

type instance ElemT  (TreeMap k a) = (k, a)
type instance IndexT (TreeMap k a) = k
type instance ValueT (TreeMap k a) = a

instance Ord k => ITraversable (TreeMap k a) where

    traverseIndexed :: forall g. Applicative g => (k -> a -> g a) -> TreeMap k a -> g (TreeMap k a)
    traverseIndexed = TreeMap.traverseWithKey

    mkElement :: Proxy (TreeMap k a) -> k -> a -> (k, a)
    mkElement _ k v = (k, v)

    unElement :: Proxy (TreeMap k a) -> (k, a) -> a
    unElement _ (k, v) = v

    -- Overrides, the native impl may be faster

    ofoldMap = foldMap  -- TreeMap implements Data.Foldable
    omap = TreeMap.map
    lookup :: k -> TreeMap k a -> Maybe a
    lookup = TreeMap.lookup

-- ** HashMap

type instance ElemT  (HashMap k a) = (k, a)
type instance IndexT (HashMap k a) = k
type instance ValueT (HashMap k a) = a

instance (Eq k, Hashable k) => ITraversable (HashMap k a) where

    traverseIndexed :: forall g. Applicative g => (k -> a -> g a) -> HashMap k a -> g (HashMap k a)
    traverseIndexed = HashMap.traverseWithKey

    mkElement :: Proxy (HashMap k a) -> k -> a -> (k, a)
    mkElement _ k v = (k, v)

    unElement :: Proxy (HashMap k a) -> (k, a) -> a
    unElement _ (k, v) = v

    -- Overrides, the native impl may be faster

    ofoldMap = foldMap  -- HashMap implements Data.Foldable
    omap = HashMap.map
    lookup :: k -> HashMap k a -> Maybe a
    lookup = HashMap.lookup
