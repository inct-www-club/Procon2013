{-# LANGUAGE LambdaCase #-}
import Graphics.UI.FreeGame
import Data.Karakuri
import Control.Lens
import Control.Monad.State
import Control.Monad.Free
import System.Environment
import Control.Comonad
import Butai

draggablePoint :: (Monad m, Figure2D m, Mouse m) => Vec2 -> Karakuri m Vec2
draggablePoint _p = fst <$> stateful go (_p, Nothing)  where
    rect = BoundingBox (-6) (-6) 6 6
    go = do
        (q, s) <- get
        p <- mousePosition
        colored (blue & _Alpha .~ 0.5) $ translate q $ polygon $ rect ^.. _Corners
        case s of
            Nothing -> whenM mouseButtonL $ when (inBoundingBox (p - q) rect) $ put (q, Just (q - p))
            Just r -> do
                unlessM mouseButtonL $ _2 .= Nothing
                _1 .= p + r

button :: (Monad m, Figure2D m, Mouse m) => Karakuri m Bool
button = (\(x, y) -> not x && y) <$> stateful go (False, False) where
    rect = BoundingBox (-16) (-16) 16 16
    go = do
        p <- mousePosition
        col <- bool red yellow `liftM` mouseButtonL
        colored (col & _Alpha .~ 0.5) $ polygon $ rect ^.. _Corners
        _1 <~ use _2
        _2 <~ return (inBoundingBox p rect) <&=> mouseButtonL

main = getArgs >>= \case
    (path : _) -> runGame def $ runButaiT $ do
        ks <- sequence
            [register $ draggablePoint (V2 200 200)
            , register $ draggablePoint (V2 280 200)
            , register $ draggablePoint (V2 280 280)
            , register $ draggablePoint (V2 200 280)
            ]
        btn <- register $ transKarakuri (translate (V2 80 80)) button
        bmp <- loadBitmapFromFile path
        forever $ do
            fromBitmap bmp
            updateAll
            ps <- mapM look ks
            colored red $ polygonOutline ps
            lift tick