
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
{-# LANGUAGE DefaultSignatures      #-}

module Container.Constrained where

import qualified Prelude as P
import Data.Kind (Type, Constraint)
import Data.Functor.Identity (Identity)
import Control.Applicative (Const)

class Constrained (f :: Type -> Type) where
    type Dom f (a :: Type) :: Constraint
    type Dom f a = ()

class Constrained f => CFunctor f where
  cmap :: (Dom f a, Dom f b) => (a -> b) -> f a -> f b
  default cmap :: P.Functor f => (a -> b) -> f a -> f b
  cmap = P.fmap
  {-# INLINE cmap #-}

instance Constrained Identity
instance CFunctor Identity
instance Constrained (Const m)
instance CFunctor (Const m)
instance Constrained ((->) r)
instance CFunctor ((->) r)
instance Constrained []
instance CFunctor []
instance Constrained P.Maybe
instance CFunctor P.Maybe
instance Constrained ((,) a)
instance CFunctor ((,) a)
instance Constrained P.IO
instance CFunctor P.IO
