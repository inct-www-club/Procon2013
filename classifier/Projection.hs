{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module Projection where

import Data.Word
import Linear
import Control.Monad
import Control.Lens

import qualified Data.Array.Repa as R
import qualified Data.Array.Unboxed as A
import qualified Data.Array.ST as A

data Quadrangle a = Quadrangle (V2 a) (V2 a) (V2 a) (V2 a) deriving Show

crop :: R.Source r Word8 => Quadrangle Float -> V2 Int -> R.Array r R.DIM3 Word8 -> A.UArray (V2 Int) Int
crop q size@(V2 w h) ar = A.runSTUArray $ do
    let rn = (V2 0 0, V2 (w - 1) (h - 1))
    let br (V2 i j) = (`div`3) $ fromIntegral (ar R.! (R.Z R.:. j R.:. i R.:. 1))
            + fromIntegral (ar R.! (R.Z R.:. j R.:. i R.:. 2))
            + fromIntegral (ar R.! (R.Z R.:. j R.:. i R.:. 3))

    m <- A.newArray_ rn
    forM_ (A.range rn) $ \pos -> do
        let src = (fmap fromIntegral pos / fmap fromIntegral size) ^. from (projecting q)
        A.writeArray m pos $ br $ fmap floor src
    return m

projecting :: (Ord a, Fractional a) => Quadrangle a -> Iso' (V2 a) (V2 a)
projecting (Quadrangle p q r s) = iso rev go where
    go (V2 u v) = p ^* ((1 - u) * (1 - v))
        + q ^* (u * (1 - v))
        + r ^* ((1 - u) * v)
        + s ^* (u * v)

    rev t = search (V2 0 0) (V2 1 0) (V2 0 1) (V2 1 1) where
        search a b c d
            | qd a d < 1e-6 = a
            | ta == tmin = search a         (mid a b) (mid a c) (mid a d)
            | tb == tmin = search (mid b a) b         (mid b c) (mid b d)
            | tc == tmin = search (mid c a) (mid c b) c         (mid c d)
            | td == tmin = search (mid d a) (mid d b) (mid d c) d where
                ta = qd t (go a)
                tb = qd t (go b)
                tc = qd t (go c)
                td = qd t (go d)
                tmin = minimum [ta, tb, tc, td]
                mid f g = (f + g) / 2
