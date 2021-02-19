
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
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DefaultSignatures      #-}

module Container.Container2
    (
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
import Data.Functor (Functor)

-- like container but poly only

{-
Reasoning for not using monomorphics:
    - Can't "sequenceA" at all
        useless for monadic operations etc?
        how about converting them to a more general map?
    - "fmap" is kinda useless

But:
    sets are MONO, a -> ()
    need keymap for sets
    -> Sets are not ok, use "Map a ()" instead.

-}

-- Definition of Traversal just like Control.Lens
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

class Functor c => Container (c :: Type -> Type) where
    type IndexT c a :: *
    type IndexC c (a :: Type) :: Constraint
    type IndexC c a = Eq (IndexT c a)           -- We need comparisons for keys

    traverseIndexed :: (Applicative g) => (IndexT c a -> a -> g b) -> c a -> g (c b)

    -- You can use this to implement "Traversable"
    traverseDefault :: (Applicative g) => (a -> g b) -> c a -> g (c b)
    traverseDefault = traverseIndexed . const

    -- Folds for free

    foldMapIndexed :: forall a m. (Monoid m) => (IndexT c a -> a -> m) -> c a -> m
    foldMapIndexed f = getConst . traverseIndexed f' where
        f' :: IndexT c a -> a -> Const m a
        f' = (Const .) . f
    
    foldMap :: (Monoid m) => (a -> m) -> c a -> m
    foldMap = foldMapIndexed . const

    -- Compliance with Control.Lens.At

    --ixDefault :: forall a. (IndexC c a) => IndexT c a -> Traversal' (c a) a
    ixDefault :: forall a. (IndexC c a) => IndexT c a ->
        (forall f. Applicative f => (a -> f a) -> c a -> f (c a))
    
    default ixDefault :: forall a. (Eq (IndexT c a)) => IndexT c a -> Traversal' (c a) a
    ixDefault key f = traverseIndexed blah where
        --blah :: IndexT c a -> a -> f a  -- can't write f here, it's inside the "Traversal" type
        blah k v  | k == key  = f v
        blah _ v              = pure v

    -- Lookup for free, if index constraint is "Eq (IndexT c a)"

    lookup :: forall a. (IndexC c a) => IndexT c a -> c a -> Maybe a
    -- default lookup :: forall a. (Eq (IndexT c a)) => IndexT c a -> c a -> Maybe a
    lookup k = getFirst . getConst . ixDefault k pure

    -- lookup i = getFirst . foldMapIndexed match where
    --     match :: IndexT c a -> a -> First a
    --     match j v | i == j  = First (Just v)
    --     match _ _           = mempty

instance Container [] where
    type IndexT [] a = Natural
    traverseIndexed :: forall g a b. (Applicative g) => (Natural -> a -> g b) -> [a] -> g [b]
    traverseIndexed g = go 0 where
        go :: Natural -> [a] -> g [b]
        go i []     = pure []
        go i (x:xs) = (:) <$> g i x <*> traverseIndexed g xs

instance Container (HashMap k) where
    type IndexT (HashMap k) a   = k

    traverseIndexed :: (Applicative g) => (k -> a -> g b) -> HashMap k a -> g (HashMap k b)
    traverseIndexed = traverseWithKey
