
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
    (   IndexT
    ,   IndexC
    ,   Container(..)
    ) where

import Prelude (($),(.), Eq(..), Int, Bool(..), const, Show, id)
import Data.Kind (Type, Constraint)
import Control.Applicative (Applicative(..), (<$>), (<*>), Const(..))
import Data.Monoid
import Numeric.Natural (Natural)
import Data.Maybe (Maybe (Just), isJust)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap, traverseWithKey)
import Data.Hashable (Hashable)

import Control.Subcategory.Functor (Dom, Constrained(..), CFunctor(..))

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

available lenses:

    type SimpleLens t a = forall f. Functor f => (a -> f a) -> (t -> f t)
    type SimpleTraversal t a = forall f. Applicative f => (a -> f a) -> (t -> f t)
    -- Traversal can have multiple or no hits

    type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
    type Lens s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

    Our polymorphic containers have:
        atKey :: k -> Maybe (Lens (c a) (c a) a a)  -- because the key might be absent
        atKeyT :: k -> Traversal  (c a) (c a) a a
        atAllKeys :: Traversal (c a) (c b) a b

how does alter / alterF work?
    alterF :: (Functor f, KeyDom k)
       => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
    looks a bit like a traverse? (for one specific key)
    notes that it is a version of the "at" combinator from Control.Lens.At
    it allows the creation of ANY key k
    so it only works for "True" maps

remapping keys
    remapping existing keys
        should be possible for all containers
        how to represent permutations over key set?
    remapping to new keys
        only for "true maps"
        lookup = alterF (Const)
        insert = alterF (Identity)

todo:
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

-- It's generally advantageous to have "property-like" type classes
--  outside of the class defintion
type family IndexT (c :: Type) :: Type
type family IndexC (f :: Type -> Type) :: Constraint

class CFunctor f => Container (f :: Type -> Type) where
    --type IndexT (f a) :: *
    --type ValueT f (a :: Type) :: *    -- always equal to a
    -- type IndexC f (a :: Type) :: Constraint
    -- type IndexC f = Eq (IndexT (f a))

    traverseIndexed :: (Dom f a, Dom f b, Applicative g) => (IndexT (f a) -> a -> g b) -> f a -> g (f b)

    -- almost free: you'll really want to override this, it's slow

    {- TODO: Should we even consider a type to be a container,
        if we can't even lookup things? This makes little sense now!
        E.g. making a HashMap of a non-hashable key: "HashMap (a -> b) v"
        Isn't it just smarter to have the key constraint as a
        constraint for the instance? E.g.:
            instance Hashable k -> Container (HashMap k)
    -}
    lookup :: (Dom f a, IndexC f) => IndexT (f a) -> f a -> Maybe a
    
    default lookup :: forall a. (Dom f a, Eq (IndexT (f a))) => IndexT (f a) -> f a -> Maybe a
    lookup i = getFirst . foldMapIndexed match where
        match :: IndexT (f a) -> a -> First a
        match j v | i == j  = First (Just v)
        match _ _           = mempty

    -- free stuff

    traverse :: (Dom f a, Dom f b, Applicative g) => (a -> g b) -> f a -> g (f b)
    traverse = traverseIndexed . const

    foldMapIndexed :: forall a m. (Dom f a, Monoid m) => (IndexT (f a) -> a -> m) -> f a -> m
    foldMapIndexed f = getConst . traverseIndexed f' where
        f' :: IndexT (f a) -> a -> Const m a
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

type instance IndexT [a] = Natural
type instance IndexC [] = ()

instance Container [] where
    traverseIndexed :: forall g a b. (Applicative g) => (Natural -> a -> g b) -> [a] -> g [b]
    traverseIndexed g = go 0 where
        go :: Natural -> [a] -> g [b]
        go i []     = pure []
        go i (x:xs) = (:) <$> g i x <*> traverseIndexed g xs


-- instance Container (HashSet) where
--     type IndexT HashSet a   = a

--     -- Dumb impl    -- todo: remove overhead, remove Eq b!
--     traverseIndexed :: forall g a b. (Hashable a, Hashable b, Eq b, Applicative g) => (a -> a -> g b) -> HashSet a -> g (HashSet b)
--     traverseIndexed g = (<$>) HashSet.fromList
--         . traverseIndexed (\_ k -> g k k)
--         . HashSet.toList

type instance IndexT (HashMap k v) = k
type instance IndexC (HashMap k) = (Eq k, Hashable k)

instance Container (HashMap k) where
    traverseIndexed :: (Applicative g) => (k -> a -> g b) -> HashMap k a -> g (HashMap k b)
    traverseIndexed = traverseWithKey

-- Sets are "a little special"
newtype WrapSet set k v = WrapSet { unWrapSet :: set k }

type instance IndexT (WrapSet set k v) = k

-- withMap :: (HashMap k () -> HashMap k ()) -> WrapSet HashSet k a -> WrapSet HashSet k a
-- withMap f = WrapSet . HashSet.fromMap . f . HashSet.toMap . unWrapSet

instance Constrained (WrapSet set k) where
  type Dom (WrapSet set k) a = a ~ ()

instance CFunctor (WrapSet HashSet k) where
    cmap :: (() -> ()) -> WrapSet HashSet k () -> WrapSet HashSet k ()
    cmap _ = id

type instance IndexC (WrapSet HashSet k) = (Eq k, Hashable k)

instance Container (WrapSet HashSet k) where
    traverseIndexed :: (Applicative g) => (k -> () -> g ()) -> WrapSet HashSet k () -> g (WrapSet HashSet k ())
    traverseIndexed f (WrapSet s) = WrapSet . HashSet.fromMap <$> traverseWithKey f (HashSet.toMap s)

-- and checking for elements becomes:
hasElem :: (Container (WrapSet set k), IndexC (WrapSet set k)) =>
    k -> WrapSet set k () -> Bool
hasElem = (isJust .) . lookup
