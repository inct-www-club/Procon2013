{-# LANGUAGE LambdaCase, TemplateHaskell #-}
import Graphics.UI.FreeGame
import Data.Karakuri
import Control.Monad.Butai
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import System.Environment
import Control.Comonad

import Classifier 
import Projection
import Karakuri
import Util
import Tree

import Data.Reflection
import Data.Foldable (toList)

data World = World
    { _result :: [V2 Float]
    }
makeLenses ''World

blackPoint :: Int -> Pattern Float
blackPoint r = do
    let p00 = V2 (-r) (-r)
        p10 = V2 r (-r)
        p01 = V2 (-r) r
        p11 = V2 r r
        w = r `div` 2
    b <- brightness $ V2 r r
    w <- fmap ((/4) . sum) $ sequence [
          translatePattern (V2 (-r) 0) $ brightness $ V2 w r
        , translatePattern (V2 r 0) $ brightness $ V2 w r
        , translatePattern (V2 0 (-r)) $ brightness $ V2 r w
        , translatePattern (V2 0 r) $ brightness $ V2 r w
        ]
    return (b - w)

blackPoints :: Int -> V2 Int -> Pattern [V2 Float]
blackPoints r size@(V2 w h) = do
    Endo t <- fmap mconcat $ spawnPatterns [execWriterT $ forM_ [r..h-r-1] $ \j -> pat i j | i <- [r..w-r-1]]
    return
        $ map ((/fmap fromIntegral size) . fmap fromIntegral)
        $ nubNear r
        $ map getOn $ toList $ t Empty
    where
        pat i j = do
            let p = V2 i j
            s <- lift $ translatePattern p $ blackPoint r
            when (s < -0.2) $ tell $ Endo $ insert (On s p)

analyzeMain bmp = do
    
    let (imgW, imgH) = bitmapSize bmp & each %~ fromIntegral
    
    let sc = 800 / imgW

    scale (return sc) $ do

        ks <- sequence
            [register $ draggablePoint (6/sc) (V2 200 200)
            , register $ draggablePoint (6/sc) (V2 280 200)
            , register $ draggablePoint (6/sc) (V2 280 280)
            , register $ draggablePoint (6/sc) (V2 200 280)
            ]
        sl <- register $ transKarakuri (translate (V2 80 360)) (slider 120)
        btn <- register $ transKarakuri (translate (V2 80 80)) button
        forever $ do
            translate (V2 (imgW / 2) (imgH / 2)) $ fromBitmap bmp
            updateAll

            ps@[p, q, s, r] <- mapM look ks
            colored red $ polygonOutline ps
            
            let resolution = V2 (60 * 9) (60 * 5)
                radius = 12

            whenM (look btn) $ do
                let quad = Quadrangle p q r s
                    img = integrate $ crop quad resolution (view _BitmapArray bmp)

                result .= mapping (projecting quad)
                    # runPattern img (blackPoints radius resolution)
            
            vs <- use result
            forM_ vs $ \v -> translate v $ colored (yellow & _Alpha .~ 0.5) $ circle 8
            
            lift $ lift tick

main = getArgs >>= \case
    (path : _) -> do
        bmp <- loadBitmapFromFile path
        font <- loadFont "../preview/VL-PGothic-Regular.ttf"
        give font $ runGame def { _windowRegion = BoundingBox 0 0 800 600 } $ runButaiT $ evalStateT (analyzeMain bmp) (World [])
    _ -> fail "classifier [path]"