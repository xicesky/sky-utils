
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

module Container.Properties where

import Prelude
import Container.Container

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Control.Applicative

{-# ANN module "HLint: ignore Use newtype instead of data" #-}
{-# ANN module "HLint: ignore Use camelCase" #-}

prop_con_listsize :: forall f a. (Container f, Dom f a) => f a -> Property
prop_con_listsize c = l1 === l2 where
    l1 = fromIntegral $ length $ (valueList c :: [] a)
    l2 = size c

prop_container  :: (Container f, Dom f a) => f a -> Property
prop_container c = conjoin . fmap ($ c) $
    [   prop_con_listsize
    ]   -- more here soon
