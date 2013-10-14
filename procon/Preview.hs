{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, LambdaCase, MultiWayIf #-}
module Preview (previewMain) where
{-
Requires GHC >= 7.6
cabal update && cabal install free-game lens
cat /path/to/text | encoder encode | runhaskell preview.hs
-}
import Graphics.UI.FreeGame
import Control.Monad
import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Monad.State
import System.Random
import Control.Monad.Trans.Maybe
import Control.Lens
import Control.Concurrent.MVar
import System.Environment

loadBitmaps "images"

theFont = unsafePerformIO $ loadFont "VL-PGothic-Regular.ttf"

die :: (Monad m, Picture2D m) => Char -> m ()
die 'R' = fromBitmap _die_1_png
die 'W' = fromBitmap _die_2_png
die 'B' = fromBitmap _die_5_png
die '1' = fromBitmap _die_1_png
die '2' = colored (Color 1.0 0.8 0.8 1.0) $ fromBitmap _die_2_png
die '3' = colored (Color 1.0 1.0 0.5 1.0) $ fromBitmap _die_3_png
die '4' = colored (Color 0.8 1.0 0.8 1.0) $ fromBitmap _die_4_png
die '5' = colored (Color 0.8 0.8 1.0 1.0) $ fromBitmap _die_5_png
die '6' = fromBitmap _die_6_png
die c = fail $ "undefined character: " ++ show c

dieMedium :: (Monad m, Picture2D m) => Char -> m ()
dieMedium = scale (10/16) . die

sizeLarge = 96

sizeMedium = 96 * 10 / 16

data Packet = Packet {
    _meta :: String
    , _dice :: [Char]
    }
makeLenses ''Packet

data World = World {
    _packetIndex :: Int
    , _packets :: [Packet]
    , _keyL :: Bool
    , _keyR :: Bool
    }
makeLenses ''World

pickHead :: (MonadState [a] m, MonadIO m, MonadPlus m) => m a
pickHead = get >>= \r -> case r of
    (x:xs) -> put xs >> return x
    [] -> mzero

renderPacket :: (Figure2D m, MonadIO m) => Packet -> m ()
renderPacket pkt = scale 0.7 $ translate (V2 48 48) $ flip evalStateT (view dice pkt) $ do
    runMaybeT $ do
        {-
        forM_ [1,0] $ \r -> do
            forM_ [0..13] $ \c -> do
                x <- pickHead
                translate (V2 c r ^* sizeMedium - V2 18 18 + V2 0 480) $ dieMedium x
            let col = if floor r `mod` 2 == 1 then blue else green 
            thickness 2 $ colored (col & _Alpha .~ 0.7) $ line [V2 (-60) (r * sizeMedium + 462), V2 1200 (r * sizeMedium + 462)]
        -}
        forM_ [4,3,2,1,0] $ \r -> do
            forM_ [0..8] $ \c -> do
                x <- pickHead
                translate (V2 c r ^* sizeLarge) (die x)
            let col = if floor r `mod` 2 == 0 then blue else green 
            thickness 2 $ colored (col & _Alpha .~ 0.7) $ line [V2 (-60) (r * sizeLarge), V2 1200 (r * sizeLarge)]
            {-
            forM_ [0..8] $ \c -> do
                colored (Color 0.5 0.5 0.5 1)$ line [V2 (c * sizeLarge) (-60), V2 (c * sizeLarge) 480]
                -}
    return ()

renderWorld :: (MonadState World m, Figure2D m, MonadIO m) => m ()
renderWorld = do
    i <- use packetIndex
    preuse (packets . ix i) >>= maybe (return ()) renderPacket

toPackets :: String -> [Packet]
toPackets "" = []
toPackets xs = Packet { _meta = "", _dice = take 45 xs } : toPackets (drop 45 xs)

stateToMVar :: MonadIO m => MVar s -> StateT s m a -> m a
stateToMVar mv m = do
    s <- liftIO $ takeMVar mv
    (a, s') <- runStateT m s
    liftIO $ putMVar mv s'
    return a

previewMain :: (Int -> Int) -> Bool -> String -> IO ()
previewMain f window str = do
    font <- loadFont "VL-PGothic-Regular.ttf"
    runGame def { _windowed = window, _windowTitle = "TRIDE-HC++", _clearColor = black } $ flip execStateT (World
            { _packetIndex = 0, _packets = toPackets str, _keyL = False, _keyR = False }) $ foreverTick $ do
        whenM (notM (use keyL) <&=> keySpecial KeyLeft) $ packetIndex -= 1
        whenM (notM (use keyR) <&=> keySpecial KeyRight) $ packetIndex += 1
        keyL <~ keySpecial KeyLeft
        keyR <~ keySpecial KeyRight
        renderWorld

        i <- use packetIndex

        translate (V2 300 480) $ colored white $ text font 60 $ "#" ++ show i
        mpkt <- preuse $ packets . ix i
        case mpkt of
            Just pkt -> translate (V2 120 480) $ colored red $ text font 60 $ show
                $ f (i * 45 + length (_dice pkt))
            Nothing -> return ()

        whenM (keySpecial KeyEsc) quit `asTypeOf` return ()
    return ()

main = do
    args <- getArgs
    if  | "-h" `elem` args -> putStrLn "\
            \Usage: preview [-f] [-h]\n\
            \\tstdin: a sequence of dice ([RWS]*)\n\
            \\t-f Run in a full-screen mode\n\
            \\t-h Show this message\n\n\
            \\tPrevious packet: Left\n\
            \\tNext packet: Right\n\
            \\tQuit: Esc"
        | otherwise -> getLine >>= previewMain id ("-f" `notElem` args) >> return ()