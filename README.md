# sky-utils

Utilities library for my haskell projects

Currently only consists of common interfaces for containers, including
instances for many in base, containers and unordered-containers. Yes,
i just hate qualifying operators.

# Container

The following are implemented:
    - List
    - Set, Map from containers
    - HashSet, HashMap from unordered-containers

## Wishlist

I would like to have instances for:
    - Sequences: https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Sequence.html
    - Trees, e.g.: https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Tree.html#t:Tree
    - Bidirectional map - e.g.: https://hackage.haskell.org/package/bimaps
    - Arrays
    - Trees (Binary, Red-Black, AVL)
    - Heaps, e.g: http://hackage.haskell.org/package/heaps

