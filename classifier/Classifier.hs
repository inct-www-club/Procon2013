{-# LANGUAGE LambdaCase #-}
import Graphics.UI.FreeGame

import qualified Data.Array.Repa as R
import Data.Karakuri
import Control.Lens
import Control.Comonad
import Control.Monad.State
import System.Environment

newtype Pattern = Pattern (R.Array R.U R.DIM2 Double)
{-
score :: Pattern -> R.Array U DIM2 Word8 -> Double
score (Pattern pat) bmp = let (sh, f) = R.toFuncton bmp
    in R.fromFunction sh (\i@(Z R.:. y R.:. x) -> (pat R.! i - f (Z R.:. k * fromIntegral y R.:. k * fromIntegral x)) ^ 2)
-}
main = getArgs >>= \case
    (path : _) -> runGame def $ flip evalStateT [draggablePoint (V2 200 200)
        , draggablePoint (V2 280 200)
        , draggablePoint (V2 280 280)
        , draggablePoint (V2 200 280)]
        $ do
            bmp <- loadBitmapFromFile path
            foreverTick $ do
                fromBitmap bmp
                get >>= mapM (lift . step) >>= put
                ps <- use (to (map extract))
                colored red $ polygonOutline ps

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
