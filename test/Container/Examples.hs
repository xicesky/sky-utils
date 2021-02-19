
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

module Container.Examples where

import Prelude
import Control.Applicative

import Data.MonoTraversable

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Container.Container
import Container.Mono
import Container.Properties

{-# ANN module "HLint: ignore Use newtype instead of data" #-}
{-# ANN module "HLint: ignore Use camelCase" #-}

dup :: a -> (a, a)
dup a = (a, a)

flup :: Applicative m => (m a, m b) -> m (a, b)
flup (ma, mb) = (,) <$> ma <*> mb
-- flup = uncurry $ liftA2 (,)

schlup :: (a, a) -> [a]
schlup (a, b) = [a,b]

-- Demo for monomorphic, fixed size container
data IPair = IPair { unIPair :: (Int, Int) }
    deriving (Show, Eq, Ord)

type instance Element IPair = Int

instance MonoFunctor IPair where
    omap = omapDefault

instance MonoFoldable IPair where
    ofoldMap = ofoldMapDefault
    ofoldr = ofoldrDefault
    ofoldl' = ofoldl'Default
    ofoldr1Ex = ofoldr1ExDefault
    ofoldl1Ex' = ofoldl1Ex'Default

instance MonoTraversable IPair where
    otraverse = otraverseDefault

instance MonoContainer IPair where
    type MonoIndex IPair = Bool
    type MonoIndexC IPair = ()

    otraverseIndexed :: forall g. Applicative g =>
        (Bool -> Int -> g Int) -> IPair -> g IPair
    otraverseIndexed g = go where
        go :: IPair -> g IPair
        go (IPair (a, b)) = (IPair .) . (,) <$> g True a <*> g False b

instance Arbitrary IPair where
    arbitrary = IPair <$> (flup . dup) arbitrary

instance Arbitrary (WrapMono IPair Int) where
    arbitrary = WrapMono <$> (arbitrary :: Gen IPair)

prop_ipair_valueList :: IPair -> Bool
prop_ipair_valueList p = valueList (WrapMono p) == schlup (unIPair p)

spec_ipair :: Spec
spec_ipair = do
    prop "has size 2" $ \(p :: WrapMono IPair a) -> size p === 2
    prop "is a proper container" $ prop_ipair_valueList
        .&&. (prop_container :: WrapMono IPair Int -> Property)
