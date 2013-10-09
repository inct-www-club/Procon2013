{-# LANGUAGE Rank2Types, FlexibleContexts, DeriveFunctor, ExistentialQuantification #-}
module Classifier where
import qualified Data.Array.Repa as R

import Control.Lens
import Control.Comonad
import Control.Monad.State
import qualified Data.Array.Unboxed as A
import qualified Data.Array.ST as A
import Control.Monad.Free
import Data.Foldable (toList)
import Linear
import Data.Monoid
import Control.Monad.Writer
import Tree
import Data.Word
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe

newtype IntegratedImage = IntegratedImage (A.UArray (V2 Int) Int)

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

integrate :: A.UArray (V2 Int) Int -> IntegratedImage
integrate ar = IntegratedImage $ A.runSTUArray $ do
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

regionBrightness :: IntegratedImage
    -> V2 Int -- Size
    -> V2 Int -- Coordinate
    -> Int
regionBrightness (IntegratedImage ar) s@(V2 w h) i = m ar where
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

matchBase :: V2 Int -> IntegratedImage -> PatternBase a -> a
matchBase p img (Rectangle s f) = f (regionBrightness img s p)
matchBase p img (Translate t pat) = matchBase (t + p) img pat
matchBase p img (Spawn ps cont) = cont $ unsafePerformIO $ do
    v <- newEmptyMVar
    forM_ ps $ \pat -> forkIO $ putMVar v $ iter (matchBase p img) pat
    forM ps $ const $ takeMVar v

translate :: V2 Int -> Pattern a -> Pattern a
translate t = hoistFree (Translate t)

averageBrightness :: V2 Int -> Pattern Float
averageBrightness s@(V2 w h) = liftF $ Rectangle s ((/fromIntegral (w * h * 256)) . fromIntegral)

match :: IntegratedImage -> Pattern a -> a
match img = iter (matchBase zero img)

spawn :: [Pattern a] -> Pattern [a]
spawn ps = liftF $ Spawn ps id

blackPoint :: Int -> Pattern Float
blackPoint r = do
    let p00 = V2 (-r) (-r)
        p10 = V2 r (-r)
        p01 = V2 (-r) r
        p11 = V2 r r
        w = r `div` 2
    b <- averageBrightness $ V2 r r
    w <- fmap ((/4) . sum) $ sequence [
          translate (V2 (-r) 0) $ averageBrightness $ V2 w r
        , translate (V2 r 0) $ averageBrightness $ V2 w r
        , translate (V2 0 (-r)) $ averageBrightness $ V2 r w
        , translate (V2 0 r) $ averageBrightness $ V2 r w
        ]
    return (b - w)

data On b a = On b a

getOn :: On b a -> a
getOn (On _ a) = a

instance Eq b => Eq (On b a) where
    On x _ == On y _ = x == y

instance Ord b => Ord (On b a) where
    compare (On x _) (On y _) = compare x y 

blackPoints :: Int -> V2 Int -> Pattern [V2 Float]
blackPoints r size@(V2 w h) = do
    Endo t <- execWriterT $ forM_ [r..w-r-1] $ \i -> do
        let go j = execWriterT $ do
                let p = V2 i j
                s <- lift $ translate p $ blackPoint r
                when (s < -0.2) $ tell $ Endo $ insert (On s p)
        f <- lift $ fmap mconcat $ spawn $ map go [r..h-r-1]
        tell f 

    return
        $ map ((/fmap fromIntegral size) . fmap fromIntegral)
        $ nubNear r
        $ map getOn $ toList $ t Empty

nubNear :: (Num a, Ord a) => a -> [V2 a] -> [V2 a]
nubNear r (v : vs) = v : nubNear r (filter ((>r^2) . qd v) vs)
nubNear r [] = []

projecting :: forall a. (Ord a, Fractional a) => Quadrangle a -> Iso' (V2 a) (V2 a)
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


unitSquare :: Num a => Quadrangle a
unitSquare = Quadrangle (V2 0 0) (V2 1 0) (V2 0 1) (V2 1 1)
