import qualified Data.Array.Repa as R

import Control.Lens
import Control.Comonad
import Control.Monad.State
import qualified Data.Array as A
import Control.Monad.Free

newtype IntegratedImage = IntegratedImage (UArray (V2 Int) Int)

regionBrightness :: IntegratedImage
    -> V2 Int -- Size
    -> V2 Int -- Coordinate
    -> Float
regionBrightness (IntegratedImage ar) s@(V2 sx sy) i = fromIntegral (m ar) / 256 / (sx * sy) where
    V2 hx hy = s `div` 2
    m = do
        let v = fmap (maybe 0.5 id) . preview
        r <- v $ ix $ i + V2 hx hy
        a <- v $ ix $ i + V2 (-hx) hy
        b <- v $ ix $ i + V2 hx (-hy)
        c <- v $ ix $ i + V2 (-hx) (-hy)
        return $ r - a - b - c

integrate :: R.Source r Word8 => R.Array r R.DIM3 Word8 -> IntegratedImage
integrate ar = IntegratedImage $ runSTUArray $ do
    let (Z R.:. h R.:. w R.:. _, f) = R.toFuncton ar
    let br (V2 i j) = (`div`3) $ fromIntegral $ f (Z R.:. j R.:. i R.:. 0) + f (Z R.:. j R.:. i R.:. 1) + f (Z R.:. j R.:. i R.:. 2)
    
    m <- newArray_ (V2 0 0, V2 (h - 1) (w - 1))

    forM_ [0..w-1] $ \i -> do
        let x = V2 i 0
        writeArray m x $ br x
    
    forM_ [0..h-1] $ \j -> do
        let x = V2 0 j
        writeArray m x $ br x
    
    forM_ [1..w-1] \i -> $ forM_ [1..h-1] $ \j -> do
        a <- readArray m (V2 (i - 1) j)
        b <- readArray m (V2 i (j - 1))
        writeArray m (V2 i j) $ br i j + b + c

    return m

data PatternBase a = Rectangle (V2 Int) (Int -> a) | Translate (V2 Int) (Pattern a)

matchBase :: V2 Int -> IntegratedImage -> PatternBase a -> a
matchBase p img (Rectangle s f) = f (regionBrightness img s p)
matchBase p img (Translate t pat) = match (t + p) pat img

type Pattern = Free PatternBase

translate :: V2 Int -> Pattern a -> Pattern a
translate t = hoistFree (Translate t)

averageBrightness :: V2 Int -> Pattern Float
averageBrightness s@(V2 w h) = liftF $ Rectangle s ((/(sx * sy * 256)) . fromIntegral)

match :: V2 Int -> IntegratedImage -> Pattern a -> a
match p img = iter matchBase

blackPoint :: Int -> Pattern Float
blackPoint r = do
    let p00 = V2 (-r) (-r)
        p10 = V2 r (-r)
        p01 = V2 (-r) r
        p11 = V2 r r
        w = r * 0.5
    b <- averageBrightness $ V2 r r
    w <- fmap ((/4) . sum) $ sequence [
          translate (V2 (-r) 0) $ averageBrightness $ V2 w r
        , translate (V2 r 0) $ averageBrightness $ V2 w r
        , translate (V2 0 (-r)) $ averageBrightness $ V2 r w
        , translate (V2 0 r) $ averageBrightness $ V2 r w
        ]
    return (b - w)

points :: Int -> V2 Int -> Pattern (Tree Float (V2 Int))
points r (V2 w h) = fmap (`appEndo` Bin 0.5 Empty Empty) $ execWriterT $ forM (range (V2 r r, V2 (w - r - 1) (h - r - 1))) $ \p -> do
    s <- lift $ translate p $ blackPoint r
    tell $ Endo $ append s p

nubNear :: Num a => a -> [V2 a] -> [V2 a]
nubNear r (v : vs) = v : nubNear r (filter ((>r^2) . qd v) vs)
nubNear r [] = []
