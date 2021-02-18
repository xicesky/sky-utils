
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
    ,   WrapMono(..)
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
import Data.Coerce

import Data.MonoTraversable (MonoTraversable, Element)
import Control.Subcategory.Functor (WrapMono(..))

import Container.Container
--import Container.Constrained

class MonoTraversable c => MonoContainer (c :: Type) where
    type MonoIndex c :: Type

    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverseMonoIndexed :: Applicative g =>
        (MonoIndex c -> Element c -> g (Element c)) -> c -> g c

instance MonoContainer c => Container (WrapMono c) where
    type IndexT (WrapMono c) a = MonoIndex c

    traverseIndexed :: forall g a b. (a ~ Element c, b ~ Element c, Applicative g) =>
        (MonoIndex c -> Element c -> g (Element c)) -> WrapMono c a -> g (WrapMono c b)
    -- traverseIndexed :: forall g a b. (a ~ MonoElement c, b ~ MonoElement c, Applicative g) =>
    --     (MonoIndex c -> a -> g a) -> WrapMono c a -> g (WrapMono c b)
    -- traverseIndexed :: forall g a b. (Applicative g) =>
    --     (MonoIndex c -> a -> g a) -> WrapMono c a -> g (WrapMono c a)
    -- traverseIndexed g = coerce $ traverseMonoIndexed g
    traverseIndexed g = (<$>) WrapMono . traverseMonoIndexed g . unwrapMono
