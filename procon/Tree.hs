{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Tree where
import Prelude hiding (foldr)
import Data.Foldable
import Data.Traversable
import Data.Monoid

data Tree a = Empty | Bin (Tree a) a (Tree a) deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

insert :: Ord a => a -> Tree a -> Tree a
insert a Empty = Bin Empty a Empty
insert a (Bin l b r)
    | a < b = Bin (insert a l) b r
    | otherwise = Bin l b (insert a r)

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Empty
{-# INLINE fromList #-}

instance Ord a => Monoid (Tree a) where
    mempty = Empty
    {-# INLINE mempty #-}
    mappend Empty z = z
    mappend z Empty = z
    mappend s t = foldr insert t s

testCommutativity :: Ord a => [a] -> [a] -> Bool
testCommutativity xs ys = fromList xs <> fromList ys == fromList ys <> fromList xs

testAssociativity :: Ord a => [a] -> [a] -> [a] -> Bool
testAssociativity xs ys zs = (fromList xs <> fromList ys) <> fromList zs == fromList xs <> (fromList ys <> fromList zs)