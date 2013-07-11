{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, LambdaCase #-}
import Graphics.UI.FreeGame
import Control.Monad
import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Monad.State
import System.Random
import Control.Monad.Trans.Maybe
import UI.PadKontrol
import Control.Lens
import Control.Concurrent.MVar

loadBitmaps "images"

theFont = unsafePerformIO $ loadFont "VL-PGothic-Regular.ttf"

die :: Picture2D m => Char -> m ()
die 'R' = fromBitmap _die_1_png
die 'W' = fromBitmap _die_2_png
die 'B' = fromBitmap _die_5_png

data Packet = Packet {
    _meta :: String
    , _dice :: [Char]
    }
makeLenses ''Packet

data World = World {
    _packetIndex :: Int
    , _packets :: [Packet]
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
                translate (V2 (c * 96) (r * 96)) (die x)
        forM_ [0..1] $ \r -> do
            forM_ [0..13] $ \c -> do
                x <- pickHead
                translate (V2 (c * 96 * 10 / 16 - 18) (r * 96 * 10 / 16 + 480 - 18)) $ scale (10/16) (die x)
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

handle mv _ JogCW  = stateToMVar mv $ packetIndex += 1
handle mv _ JogCCW  = stateToMVar mv $ packetIndex -= 1
handle mv _ (PadDown p _) = stateToMVar mv $ packetIndex .= fromEnum p
handle _ _ _ = return ()

main = do
    pkts <- toPackets <$> getLine
    mv <- newMVar $ World { _packetIndex = 0, _packets = pkts }
    runGame def $ do
        foreverTick $ stateToMVar mv renderWorld