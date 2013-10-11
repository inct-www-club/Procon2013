{-# LANGUAGE LambdaCase, TemplateHaskell, BangPatterns, FlexibleContexts #-}

import Graphics.UI.FreeGame
import Data.Karakuri
import Control.Monad.Butai
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import System.Environment
import Control.Comonad

import Recognition
import Projection
import Karakuri
import Util
import Tree

import Encoding

import Data.Reflection
import Data.Foldable (toList)
import Control.Concurrent.MVar
import Control.Concurrent
{-
import Data.Aeson (Value)
import Data.Aeson.Lens
-}

data World = World
    { _theGrid :: (Int, Int)
    , _theThreads :: [ThreadId]
    }
makeLenses ''World

blackPoint :: Int -> Int -> Int -> Pattern Float
blackPoint m w r = do
    let hw = w `div` 2 + m
    b <- brightness $ V2 r r
    w <- fmap ((/4) . sum) $ sequence [
          translatePattern (V2 (-r - hw) 0) $ brightness $ V2 w r
        , translatePattern (V2 (r + hw) 0) $ brightness $ V2 w r
        , translatePattern (V2 0 (-r - hw)) $ brightness $ V2 r w
        , translatePattern (V2 0 (r + hw)) $ brightness $ V2 r w
        ]
    return $ b - w 

dice :: Float -> Int -> Int -> Pattern [V2 Float]
dice t size resolution = do
    let res f = floor $ fromIntegral resolution * f
    s <- translatePattern (return $ res 0.5) $ blackPoint 5 (res 0.16) (res 0.3)
    if s < -0.25
        then return [return 0.5]
        else patterns size (return resolution) $ fmap (\s -> (s + t, size)) $ blackPoint 0 3 size

nubNear :: (Num a, Ord a) => [(V2 a, a)] -> [V2 a]
nubNear ((v, size) : vs) = v : nubNear (filter (\(w, s) -> qd v w > (size+s)^2) vs)
nubNear [] = []

patterns :: Int -> V2 Int -> Pattern (Float, Int) -> Pattern [V2 Float]
patterns r size@(V2 w h) pat = do
    Endo t <- execWriterT $ forM_ [r,r+2..h-r-1] $ \j -> forM_  [r,r+2..w-r-1] $ \i -> do
        (s, size) <- lift $ translatePattern (V2 i j) pat
        when (s < 0) $ tell $ Endo $ insert (On s (V2 i j, size))
    return
        $ map ((/fmap fromIntegral size) . fmap fromIntegral)
        $ nubNear
        $ map getOn $ toList $ t Empty

analyzeMain :: Given Font => Bitmap -> StateT World Game ()
analyzeMain bmp = runButaiT $ do
    
    let (imgW, imgH) = bitmapSize bmp & each %~ fromIntegral
    
    let sc = 800 / imgW
    
    let uiSection = scale (return (1/sc)) . translate (V2 0 500) 

    sizeSlider <- translate (V2 600 500) $ register
        $ slider 120 0 1 0.12
    resolutionSlider <-  translate (V2 600 540) $ register
        $ slider 120 60 400 120
    thresholdSlider <- translate (V2 600 580) $ register
        $ slider 120 0 1 0

    btn <- translate (V2 300 500) $ register $ button (BoundingBox 0 (-40) 120 0) (lift $ text given 32 "Analyze")
    btnReset <- translate (V2 300 550) $ register $ button (BoundingBox 0 (-40) 120 0) (lift $ text given 32 "Reset")

    setSingle <- translate (V2 60 500) $ register $ button (BoundingBox 0 (-30) 100 0) (lift $ text given 24 "Single")
    setGridL <- translate (V2 60 532) $ register $ button (BoundingBox 0 (-30) 100 0) (lift $ text given 24 "Grid[L]")
    setGridM <- translate (V2 60 564) $ register $ button (BoundingBox 0 (-30) 100 0) (lift $ text given 24 "Grid[M]")

    btnWrite <- translate (V2 200 564) $ register $ button (BoundingBox 0 (-30) 100 0) (lift $ text given 24 "Write")

    scale (return sc) $ do

        ks <- sequence
            [register $ draggablePoint (6/sc) (V2 200 200)
            , register $ draggablePoint (6/sc) (V2 1000 200)
            , register $ draggablePoint (6/sc) (V2 1000 1000)
            , register $ draggablePoint (6/sc) (V2 200 1000)
            ]

        results <- lift $ lift $ embedIO $ newMVar Empty

        forever $ do
            translate (V2 (imgW / 2) (imgH / 2)) $ fromBitmap bmp
            updateAll

            resolution <- floor <$> look resolutionSlider
            size <- look sizeSlider
            threshold <- look thresholdSlider

            lift $ lift $ uiSection $ do
                translate (V2 500 0) $ colored black $ text given 32 $ "S:" ++ show size
                translate (V2 500 40) $ colored black $ text given 32 $ "R:" ++ show resolution
                translate (V2 500 80) $ colored black $ text given 32 $ "T:" ++ show threshold

            (rows, cols) <- uses theGrid (each %~ fromIntegral)
 
            [p, q, s, r] <- mapM look ks
            let proj = getProjection $ Quadrangle p q r s

            qs <- lift $ lift $ fmap concat $ forM [0..rows - 1] $ \r -> forM [0..cols - 1] $ \c -> do
                let p' = project proj $ V2 (c / cols) (r / rows)
                    q' = project proj $ V2 ((c + 1) / cols) (r / rows)
                    r' = project proj $ V2 (c / cols) ((r + 1) / rows)
                    s' = project proj $ V2 ((c + 1) / cols) ((r + 1) / rows)
                polygonOutline [p', q', s', r']
                return ((r, c), Quadrangle p' q' r' s')
            
            whenM (look setGridM) $ theGrid .= (2, 14)
            whenM (look setGridL) $ theGrid .= (5, 9)
            whenM (look setSingle) $ theGrid .= (1, 1)

            whenM (look btnReset) $ lift $ liftIO $ swapMVar results Empty
            whenM (look btn) $ lift $ do
                liftIO $ swapMVar results Empty
                use theThreads >>= mapM_ (liftIO . killThread)

                ts <- embedIO $ forM qs $ \(p, q) -> forkIO $ do
                    let proj' = getProjection q
                    let img = integrate $ crop proj' (return resolution) (view _BitmapArray bmp)
                    let s = floor $ fromIntegral resolution * size
                    let !cs = map (project proj') $ runPattern img (dice threshold s resolution)
                    rs <- takeMVar results
                    putMVar results $ insert (On p cs) rs

                theThreads .= ts

            vs <- lift $ lift $ embedIO $ toList <$> readMVar results
            whenM (look btnWrite) $ lift $ lift $ embedIO $ do
                forM_ vs $ \(On _ v) -> putStr $ show (length v)
                putStrLn ""

            lift $ lift $ do
                thickness 2 $ colored red $ polygonOutline [p, q, s, r]

                forM_ (concatMap getOn vs) $ \v -> translate v $ colored (yellow & _Alpha .~ 0.5) $ circle 9

                tick

main = getArgs >>= \case
    (path : _) -> do
        bmp <- loadBitmapFromFile path
        font <- loadFont "VL-PGothic-Regular.ttf"
        -- Just setting <- "settings.json" ^!? act readFile . _JSON
        -- give setting $ 
        give font $ runGame def { _windowRegion = BoundingBox 0 0 800 600 }
            $ evalStateT (analyzeMain bmp) (World (5, 9) []) 
    _ -> fail "classifier [path]"