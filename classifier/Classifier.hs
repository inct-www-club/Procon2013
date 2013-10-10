{-# LANGUAGE Rank2Types, FlexibleContexts, DeriveFunctor, ExistentialQuantification #-}

module Classifier (IntegralImage
    , integrate
    , regionBrightness
    , Pattern
    , runPattern
    , brightness
    , translatePattern
    , spawnPatterns) where
import Control.Lens
import Control.Comonad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Free

import qualified Data.Array as A
import qualified Data.Array.ST as A

import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe

import Data.Monoid
import Linear

newtype IntegralImage a = IntegralImage (A.Array (V2 Int) a)

integrate :: Num a => A.Array (V2 Int) a -> IntegralImage a
integrate ar = IntegralImage $ A.runSTArray $ do
    m <- A.thaw ar
    let (V2 0 0, V2 w h) = A.bounds ar

    forM_ [1..w] $ \i -> forM_ [0..h] $ \j -> do
        a <- A.readArray m (V2 (i - 1) j)
        r <- A.readArray m (V2 i j)
        A.writeArray m (V2 i j) $ r + a

    forM_ [1..h] $ \j -> forM_ [0..w] $ \i -> do
        b <- A.readArray m (V2 i (j - 1))
        r <- A.readArray m (V2 i j)
        A.writeArray m (V2 i j) $ r + b

    return m

regionBrightness :: Num a => IntegralImage a
    -> V2 Int -- Size
    -> V2 Int -- Coordinate
    -> a
regionBrightness (IntegralImage ar) s@(V2 w h) i = m ar where
    hx = w `div` 2
    hy = h `div` 2
    m = do
        let v = fmap (maybe 0 id) . preview
        r <- v $ ix $ i + V2 hx hy
        a <- v $ ix $ i + V2 (-hx) hy
        b <- v $ ix $ i + V2 hx (-hy)
        c <- v $ ix $ i + V2 (-hx) (-hy)
        return $ r - a - b + c

type Pattern = Free PatternBase

data PatternBase a = Rectangle (V2 Int) (Int -> a)
    | Translate (V2 Int) (PatternBase a)
    | forall r. Spawn [Pattern r] ([r] -> a)

instance Functor PatternBase where
    fmap f (Rectangle v g) = Rectangle v (f . g)
    fmap f (Translate t p) = Translate t (fmap f p)
    fmap f (Spawn ps g) = Spawn ps (f . g)

matchBase :: V2 Int -> IntegralImage Int -> PatternBase a -> a
matchBase p img (Rectangle s f) = f (regionBrightness img s p)
matchBase p img (Translate t pat) = matchBase (t + p) img pat
matchBase p img (Spawn ps cont) = cont $ unsafePerformIO $ do
    v <- newEmptyMVar
    forM_ ps $ \pat -> forkIO $ putMVar v $ iter (matchBase p img) pat
    forM ps $ const $ takeMVar v

translatePattern :: V2 Int -> Pattern a -> Pattern a
translatePattern t = hoistFree (Translate t)

brightness :: V2 Int -> Pattern Float
brightness s@(V2 w h) = liftF $ Rectangle s ((/fromIntegral (w * h * 256)) . fromIntegral)

spawnPatterns :: [Pattern a] -> Pattern [a]
spawnPatterns ps = liftF $ Spawn ps id

runPattern :: IntegralImage Int -> Pattern a -> a
runPattern img = iter (matchBase zero img)
