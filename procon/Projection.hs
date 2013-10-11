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

crop :: R.Source r Word8 => Projection Float -> V2 Int -> R.Array r R.DIM3 Word8 -> A.UArray (V2 Int) Int
crop proj size@(V2 w h) ar = A.runSTUArray $ do
    let rn = (V2 0 0, V2 (w - 1) (h - 1))
    let br (V2 i j) = (`div`3) $ fromIntegral (ar R.! (R.Z R.:. j R.:. i R.:. 1))
            + fromIntegral (ar R.! (R.Z R.:. j R.:. i R.:. 2))
            + fromIntegral (ar R.! (R.Z R.:. j R.:. i R.:. 3))

    m <- A.newArray_ rn
    forM_ (A.range rn) $ \pos -> do
        let src = project proj $ fmap fromIntegral pos / fmap fromIntegral size
        A.writeArray m pos $ br $ fmap floor src
    return m

data Projection a = Projection a a a a a a a a

getProjection :: Fractional a => Quadrangle a -> Projection a
getProjection (Quadrangle (V2 x0 y0) (V2 x1 y1) (V2 x3 y3) (V2 x2 y2)) = Projection
    (x1 - x0 + g * x1)
    (x3 - x0 + h * x3)
    x0
    (y1 - y0 + g * y1)
    (y3 - y0 + h * y3)
    y0
    g
    h
    where
        sx = (x0 - x1) + (x2 - x3)
        sy = (y0 - y1) + (y2 - y3)
        dx1 = x1 - x2
        dx2 = x3 - x2
        dy1 = y1 - y2;
        dy2 = y3 - y2;
         
        z = dx1 * dy2 - dy1 * dx2
        g = (sx * dy2- sy * dx2) / z
        h = (sy * dx1 - sx * dy1) / z;

project :: Fractional a => Projection a -> V2 a -> V2 a
project (Projection p0 p1 p2 p3 p4 p5 p6 p7) (V2 u v) = V2 (p0 * u + p1 * v + p2) (p3 * u + p4 * v + p5)
    ^/ ( p6 * u + p7 * v + 1)
