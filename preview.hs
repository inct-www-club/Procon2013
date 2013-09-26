{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, LambdaCase, MultiWayIf #-}
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

die :: Picture2D m => Char -> m ()
die 'R' = fromBitmap _die_1_png
die 'W' = fromBitmap _die_2_png
die 'B' = fromBitmap _die_5_png

dieMedium :: Picture2D m => Char -> m ()
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

renderPacket :: (Picture2D m, MonadIO m) => Packet -> m ()
renderPacket pkt = scale 0.7 $ translate (V2 48 48) $ flip evalStateT (view dice pkt) $ do
    runMaybeT $ do
        forM_ [0..4] $ \r -> do
            forM_ [0..8] $ \c -> do
                x <- pickHead
                translate (V2 c r ^* sizeLarge) (die x)
        forM_ [0..1] $ \r -> do
            forM_ [0..13] $ \c -> do
                x <- pickHead
                translate (V2 c r ^* sizeMedium - V2 18 18 + V2 0 480) $ dieMedium x
    return ()

renderWorld :: (MonadState World m, Picture2D m, MonadIO m) => m ()
renderWorld = do
    i <- use packetIndex
    preuse (packets . ix i) >>= maybe (return ()) renderPacket

toPackets :: String -> [Packet]
toPackets "" = []
toPackets xs = Packet { _meta = "", _dice = take 90 xs } : toPackets (drop 90 xs)

stateToMVar :: MonadIO m => MVar s -> StateT s m a -> m a
stateToMVar mv m = do
    s <- liftIO $ takeMVar mv
    (a, s') <- runStateT m s
    liftIO $ putMVar mv s'
    return a

previewMain window = do
    pkts <- toPackets <$> getLine
    font <- loadFont "VL-PGothic-Regular.ttf"
    runGame def { _windowed = window } $ flip execStateT (World { _packetIndex = 0, _packets = pkts, _keyL = False, _keyR = False }) $ foreverTick $ do
        whenM (notM (use keyL) <&=> keySpecial KeyLeft) $ packetIndex -= 1
        whenM (notM (use keyR) <&=> keySpecial KeyRight) $ packetIndex += 1
        keyL <~ keySpecial KeyLeft
        keyR <~ keySpecial KeyRight
        renderWorld

        i <- use packetIndex
        translate (V2 100 240) $ colored (blue & _Alpha .~ 0.5) $ text font 240 $ "#" ++ show i

        whenM (keySpecial KeyEsc) quit `asTypeOf` return ()

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
        | otherwise -> previewMain ("-f" `notElem` args) >> return ()