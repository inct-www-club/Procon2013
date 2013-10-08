{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Tree where

import Data.Foldable
import Data.Monoid
import Data.Traversable

-- | Binary heap
data Tree a = Bin (Tree a) a (Tree a) | Empty deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

append :: Ord a => a -> Tree a -> Tree a
append a Empty = Bin Empty a Empty
append a (Bin l b r)
    | a < b = Bin (append a l) b r
    | otherwise = Bin l b (append a r)

