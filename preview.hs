{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
import Graphics.UI.FreeGame
import Control.Monad
import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Monad.State
import System.Random
import Control.Monad.Trans.Maybe

loadBitmaps "images"

theFont = unsafePerformIO $ loadFont "VL-PGothic-Regular.ttf"

die :: Picture2D m => Char -> m ()
die 'R' = fromBitmap _die_1_png
die 'W' = fromBitmap _die_2_png
die 'B' = fromBitmap _die_5_png

pickHead :: (MonadState String m, MonadIO m, MonadPlus m) => m Char
pickHead = get >>= \r -> case r of
    (x:xs) -> put xs >> return x
    [] -> mzero

packet :: (Monad m, Picture2D m, MonadIO m) => String -> String -> m String
packet header = execStateT $ do
    runMaybeT $ do
        forM_ [0..4] $ \c -> do
            forM_ [0..9] $ \r -> do
                x <- pickHead
                translate (V2 (c * (96 * 26/16)) (r * 96 * 10 / 16 - 18)) $ scale (10/16) (die x)
            forM_ [0..5] $ \r -> do
                x <- pickHead
                translate (V2 (c * (96 * 26/16) + 78) (r * 96)) (die x)
        forM_ [0..9] $ \r -> do
            x <- pickHead
            translate (V2 (5 * (96 * 26/16)) (r * 96 * 10 / 16 - 18)) $ scale (10/16) (die x)
    forM_ (zip [0..] header) $ \(r, ch) -> do
        translate (V2 (6 * (96 * 26/16)) (r * 96 * 6 / 16 - 20)) $ scale (6/16) (die ch)

showPackets :: [String] -> String -> Game ()            
showPackets prevs str = looper True True
    where
        looper z1 x1 = do
            translate (V2 48 470) $ colored black $ text theFont 20 $ "*" ++ (prevs >> ">")
            rest <- scale 0.6 $ translate (V2 48 48) $ packet "" str
            tick
            z <- keyChar 'Z'
            x <- keyChar 'X'
            case (not z1 && z, not x1 && x, prevs) of
                (True, False, _) -> showPackets (str:prevs) rest
                (False, True, (p:ps)) -> showPackets ps p
                _ -> looper z x
 
main = runGame def $ do
    str <- embedIO getLine
    showPackets [] str
    tick