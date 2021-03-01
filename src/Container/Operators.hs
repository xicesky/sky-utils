
module Container.Operators
    (   (<$.>)
    ,   (!?)
    ,   (!!)
    ) where

import Prelude
    (   Eq(..), Ord(..), Int, Bool(..), Show(..)
    ,   ($),(.), ($!)
    ,   id, const
    ,   fromIntegral, otherwise
    )
import Data.Maybe (Maybe, fromJust)
import Data.Function (flip)

import Container.ITraversable

-- | Monomorphic @fmap@
-- FIXME: compatibility with other libraries?
infixl 4 <$.>
(<$.>) :: ITraversable c => (ValueT c -> ValueT c) -> c -> c
(<$.>) = omap

-- | Infix operator for @lookup@
infixl 9 !?
(!?) :: ITraversable c => c -> IndexT c -> Maybe (ValueT c)
(!?) = flip lookup

-- | Unsafe version of (!?)
infixl 9 !!
(!!) :: ITraversable c => c -> IndexT c -> ValueT c
(!!) = (fromJust .) . flip lookup
