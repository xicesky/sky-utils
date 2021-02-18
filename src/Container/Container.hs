
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

module Container.Container
    (   Container(..)
    ) where

import Prelude (($),(.), Eq(..), Int, Bool(..), const, Show)
import Data.Kind (Type, Constraint)
import Control.Applicative (Applicative(..), (<$>), (<*>), Const(..))
import Data.Monoid
import Numeric.Natural (Natural)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap, traverseWithKey)
import Data.Hashable (Hashable)
import Data.Maybe (Maybe (Just))

import Control.Subcategory.Functor

{-
There are similar, but maybe mutually incompatible notions for
constrained functors (and 
Elaborate:
    21141   http://hackage.haskell.org/package/categories       (by emkmett)
    17175   http://hackage.haskell.org/package/data-category
    1981    https://hackage.haskell.org/package/supermonad-0.1
    1840    http://hackage.haskell.org/package/subhask          (experimental, prelude rewrite)
Simple:
    6585    http://hackage.haskell.org/package/ConstraintKinds  (deprecated)
    6791    https://hackage.haskell.org/package/constrained-categories-0.4.1.0
    65      https://hackage.haskell.org/package/subcategories   (on stackage)

We will be using subcategories, because of all the libraries hier it has
the smallest amount of code to achieve exactly what we need, so we can
migrate later.
What i don't like about it:
    - Even more operators
    - Big dependencies (mono-traversable), altough most other libraries
        are even worse.
-}

{-
notes for description:
    container = indexed containers = associative containers = sets
    vary by "key" and "value"
    have a POSSIBLY PARTIAL mapping "k -> v", so there must be "k -> Maybe v"
    setting a key is possible if it is already present
    but adding a key is not generally possible
    (e.g. adding key 5 to list [0,1] is impossible)

how does alter / alterF work?
    alterF :: (Functor f, KeyDom k)
       => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
    looks a bit like traverse?
    notes that it is a version of the "at" combinator from Control.Lens.At
    it allows the creation of ANY key k
    so it only works for "True" maps

remapping keys
    remapping existing keys
        should be possible for all containers
        how to represent permutations over key set
    remapping to new keys
        only for "true maps"
        lookup = alterF (Const)
        insert = alterF (Identity)

Reasoning for not using monomorphics:
    - Can't "sequenceA" at all
        useless for monadic operations etc?
        how about converting them to a more general map?
    - "fmap" is useless

todo:
    sets are MONO, a -> ()
    need keymap for sets
    -> Sets are not ok, use "Map a ()" instead.
    
    check functions (k -> v) with Enum k
    ... and enter them https://docs.google.com/spreadsheets/d/1vkPhY57SihDLNpiKepnWs_twiaOsNp9hFHy7QhJDEeU/edit#gid=0
    can partial functions be represented without using "set"?
    
    put all the default impls in a module to test against them
        if we want to use them here, they need to be in THIS module
    haddock

    check logic:
    1. any container (k -> v) that has a guaranteed lookup can be turned into a
        map or set by replacing v with "Maybe" or "Bool" respectively.

notes:
    with Enum k we can derive fold, but not traverse from just lookup
-}

class CFunctor f => Container (f :: Type -> Type) where
    type IndexT f a :: *
    --type ValueT f (a :: Type) :: *    -- always equal to a
    type IndexC f (a :: Type) :: Constraint
    type IndexC f a = Eq (IndexT f a)

    traverseIndexed :: (Dom f a, Dom f b, Applicative g) => (IndexT f a -> a -> g b) -> f a -> g (f b)

    -- almost free: you'll really want to override this, it's slow

    lookup :: (Dom f a, IndexC f a) => IndexT f a -> f a -> Maybe a
    
    default lookup :: forall a. (Dom f a, Eq (IndexT f a)) => IndexT f a -> f a -> Maybe a
    lookup i = getFirst . foldMapIndexed match where
        match :: IndexT f a -> a -> First a
        match j v | i == j  = First (Just v)
        match _ _           = mempty

    -- free stuff

    traverse :: (Dom f a, Dom f b, Applicative g) => (a -> g b) -> f a -> g (f b)
    traverse = traverseIndexed . const

    foldMapIndexed :: forall a m. (Dom f a, Monoid m) => (IndexT f a -> a -> m) -> f a -> m
    foldMapIndexed f = getConst . traverseIndexed f' where
        f' :: IndexT f a -> a -> Const m a
        f' = (Const .) . f

    foldMap :: (Dom f a, Monoid m) => (a -> m) -> f a -> m
    foldMap = foldMapIndexed . const

    -- TODO delete me
    collapseViaMonoid :: (Dom f a, Monoid m) => m -> f a -> m
    collapseViaMonoid = foldMap . const
    
    size :: (Dom f a) => f a -> Natural
    size = getSum . collapseViaMonoid (Sum 1)

    isEmpty :: (Dom f a) => f a -> Bool
    isEmpty = getAny . collapseViaMonoid (Any True)

    valueList :: (Dom f a, Applicative m, Monoid (m a)) => f a -> m a
    valueList = foldMap pure

    findValue :: (Dom f a) => (a -> Bool) -> f a -> Bool
    findValue p = getAny . foldMap (Any . p)


instance Container [] where
    type IndexT [] a = Natural
    traverseIndexed :: forall g a b. (Applicative g) => (Natural -> a -> g b) -> [a] -> g [b]
    traverseIndexed g = go 0 where
        go :: Natural -> [a] -> g [b]
        go i []     = pure []
        go i (x:xs) = (:) <$> g i x <*> traverseIndexed g xs

instance Container (HashSet) where
    type IndexT HashSet a   = a

    -- Dumb impl    -- todo: remove overhead, remove Eq b!
    traverseIndexed :: forall g a b. (Hashable a, Hashable b, Eq b, Applicative g) => (a -> a -> g b) -> HashSet a -> g (HashSet b)
    traverseIndexed g = (<$>) HashSet.fromList
        . traverseIndexed (\_ k -> g k k)
        . HashSet.toList

instance Container (HashMap k) where
    type IndexT (HashMap k) a   = k

    traverseIndexed :: (Applicative g) => (k -> a -> g b) -> HashMap k a -> g (HashMap k b)
    traverseIndexed = traverseWithKey

