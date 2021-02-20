
-- Re-export containers with unique names

module Container.Containers
    (   TreeSet
    ,   HashSet
    ,   TreeMap
    ,   HashMap
    ) where

import qualified Data.List          as List
import qualified Data.Map           as TreeMap
import qualified Data.Set           as TreeSet

import           Data.Hashable
import qualified Data.HashMap.Lazy   as HashMap
import qualified Data.HashMap.Strict as StrictHashMap
import qualified Data.HashSet        as HashSet

type TreeSet v = TreeSet.Set v
type HashSet v = HashSet.HashSet v
type TreeMap k v = TreeMap.Map k v
type HashMap k v = HashMap.HashMap k v
