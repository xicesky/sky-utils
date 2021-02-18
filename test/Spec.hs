
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}

module Main where

import Prelude
import Container.Container
import Container.Properties
import Container.Examples

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Containers" $ do
        describe "IPair" spec_ipair
