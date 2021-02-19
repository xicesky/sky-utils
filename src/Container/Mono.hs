
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
{-# LANGUAGE TypeApplications       #-}

module Container.Mono
    (   MonoContainer(..)
    -- Re-exports
    ,   WrapMono(..)
    ) where

import Prelude
    (   Eq(..), Int, Bool(..), Show(..)
    ,   ($),(.), ($!)
    ,   id, const, errorWithoutStackTrace
    )
import Data.Kind (Type, Constraint)
import Control.Applicative (Applicative(..), (<$>), (<*>), Const(..))
import Data.Monoid
import Data.Functor.Identity (Identity(..))
import Numeric.Natural (Natural)
import Data.Maybe (Maybe(..), fromMaybe)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap, traverseWithKey)
import Data.Hashable (Hashable)

import Data.MonoTraversable (MonoTraversable, Element)
import Control.Subcategory.Functor (WrapMono(..))

import Container.Container
--import Container.Constrained

-- Some "forgotten" instances for WrapMono
instance (Show c, Element c ~ a) => Show (WrapMono c a) where
    show = show . unwrapMono

class MonoTraversable c => MonoContainer (c :: Type) where
    type MonoIndex c :: Type
    type MonoIndexC c :: Constraint

    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    otraverseIndexed :: Applicative g =>
        (MonoIndex c -> Element c -> g (Element c)) -> c -> g c

    olookup :: (MonoIndexC c) => MonoIndex c -> c -> Maybe (Element c)

    default olookup :: (Eq (MonoIndex c)) => MonoIndex c -> c -> Maybe (Element c)
    olookup i = getFirst . ofoldMapIndexed match where
        match :: MonoIndex c -> Element c -> First (Element c)
        match j v | i == j  = First (Just v)
        match _ _           = mempty

    -- foldMapIndexed can be derived
    ofoldMapIndexed :: forall a m. (Monoid m) => (MonoIndex c -> Element c -> m) -> c -> m
    ofoldMapIndexed f = getConst . otraverseIndexed f' where
        f' :: MonoIndex c -> Element c -> Const m (Element c)
        f' = (Const .) . f

    -- default implementations for MonoTraversable etc.
    omapDefault :: (Element c -> Element c) -> c -> c
    omapDefault f = runIdentity . otraverseIndexed f' where
        f' :: MonoIndex c -> Element c -> Identity (Element c)
        f' _ = Identity . f

    ofoldMapDefault :: forall m. Monoid m => (Element c -> m) -> c -> m
    ofoldMapDefault f = getConst . otraverseIndexed f' where
        f' :: MonoIndex c -> Element c -> Const m (Element c)
        f' _ v = Const (f v)

    ofoldrDefault :: forall b. (Element c -> b -> b) -> b -> c -> b
    ofoldrDefault f z t = appEndo (ofoldMapDefault f' t) z where
        f' :: Element c -> Endo b
        f' = Endo . f

    ofoldl'Default :: (a -> Element c -> a) -> a -> c -> a
    ofoldl'Default f z0 xs = ofoldrDefault f' id xs z0
      where f' x k z = k $! f z x

    ofoldr1ExDefault :: (Element c -> Element c -> Element c) -> c -> Element c
    ofoldr1ExDefault f xs = fromMaybe
        (errorWithoutStackTrace "ofoldr1Ex: empty structure")
        (ofoldrDefault mf Nothing xs)
        where
            mf x m = Just (case m of
                Nothing -> x
                Just y  -> f x y)

    ofoldl1Ex'Default :: (Element c -> Element c -> Element c) -> c -> Element c
    ofoldl1Ex'Default f xs = fromMaybe
        (errorWithoutStackTrace "ofoldl1Ex': empty structure")
        (ofoldl'Default mf Nothing xs)
        where
            mf m y = Just (case m of
                Nothing -> y
                Just x  -> f x y)

    otraverseDefault :: Applicative f => (Element c -> f (Element c)) -> c -> f c
    otraverseDefault = otraverseIndexed . const

type instance IndexT (WrapMono c a) = MonoIndex c
type instance IndexC (WrapMono c) = MonoIndexC c

instance MonoContainer c => Container (WrapMono c) where

    traverseIndexed :: forall g a b. (a ~ Element c, b ~ Element c, Applicative g) =>
        (MonoIndex c -> Element c -> g (Element c)) -> WrapMono c a -> g (WrapMono c b)
    -- traverseIndexed :: forall g a b. (a ~ MonoElement c, b ~ MonoElement c, Applicative g) =>
    --     (MonoIndex c -> a -> g a) -> WrapMono c a -> g (WrapMono c b)
    -- traverseIndexed :: forall g a b. (Applicative g) =>
    --     (MonoIndex c -> a -> g a) -> WrapMono c a -> g (WrapMono c a)
    -- traverseIndexed g = coerce $ otraverseIndexed g
    traverseIndexed g = (<$>) WrapMono . otraverseIndexed g . unwrapMono

    -- Need to declare lookup, because IndexC might vary!
    lookup :: (MonoIndexC c) => MonoIndex c -> WrapMono c (Element c) -> Maybe (Element c)
    lookup k = olookup k . unwrapMono

{-
instance MonoContainer (HashSet a) where
    type MonoIndex HashSet a   = a
    -- Element (HashSet a) = a  -- Is wrong in MonoTraversable! we want ()

    -- Dumb impl    -- todo: remove overhead, remove Eq b!
    traverseIndexed :: forall g a b. (Hashable a, Hashable b, Eq b, Applicative g) => (a -> a -> g b) -> HashSet a -> g (HashSet b)
    traverseIndexed g = (<$>) HashSet.fromList
        . traverseIndexed (\_ k -> g k k)
        . HashSet.toList

-}
