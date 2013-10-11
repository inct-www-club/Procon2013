{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Karakuri
    ( draggablePoint
    , button
    , slider) where

import Data.Karakuri
import Graphics.UI.FreeGame
import Control.Monad.Butai
import Control.Lens
import Control.Monad.State

instance (Monad m, Picture2D m) => Picture2D (ButaiT m) where
    fromBitmap = lift . fromBitmap
    translate t = transButaiT (translate t)
    rotateD r = transButaiT (rotateD r)
    rotateR r = transButaiT (rotateR r)
    scale s = transButaiT (scale s)
    colored c = transButaiT (colored c)

instance (Monad m, Figure2D m) => Figure2D (ButaiT m) where
    line = lift . line
    circle = lift . circle
    circleOutline = lift . circleOutline
    polygon = lift . polygon
    polygonOutline = lift . polygonOutline
    thickness t = transButaiT (thickness t)

instance (Monad m, Mouse m) => Mouse (ButaiT m) where
    mousePosition = lift mousePosition
    mouseButtonL = lift mouseButtonL
    mouseButtonM = lift mouseButtonM
    mouseButtonR = lift mouseButtonR
    mouseWheel = lift mouseWheel

instance (Monad m, Keyboard m) => Keyboard (ButaiT m) where
    keyChar = lift . keyChar
    keySpecial = lift . keySpecial

draggablePoint :: (Monad m, Figure2D m, Mouse m) => Float -> Vec2 -> Karakuri m Vec2
draggablePoint size _p = fst <$> stateful go (_p, Nothing) where
    size' = size / 2
    rect = BoundingBox (-size') (-size') size' size'
    go = do
        (q, s) <- get
        p <- mousePosition
        colored (blue & _Alpha .~ 0.5) $ translate q $ polygon $ rect ^.. _Corners
        case s of
            Nothing -> whenM mouseButtonL $ when (inBoundingBox (p - q) rect) $ put (q, Just (q - p))
            Just r -> do
                unlessM mouseButtonL $ _2 .= Nothing
                _1 .= p + r

slider :: (Monad m, Figure2D m, Mouse m) => Float -> Float -> Float -> Float -> Karakuri m Float
slider width vmin vmax val = fmap ((vmin +) . (*r) . (/width) . (+width') . fst)
    $ stateful go ((val - vmin) / r * width - width', False) where
    width' = width / 2
    r = vmax - vmin
    go = do
        let color = blue
        p <- mousePosition
        (v, f) <- get
        colored color $ do
            translate (V2 v 0) $ polygonOutline [V2 0 0, V2 (-16) (-16), V2 16 (-16)]
            line [V2 (-width') (-16), V2 (-width') 0, V2 width' 0, V2 width' (-16)]

        if f
            then do
                unlessM mouseButtonL $ _2 .= False
                _1 .= min width' (max (-width') (p ^. _x))

            else whenM mouseButtonL $ when (p ^. _y > (-16)
                    && p ^. _y < 0
                    && p ^. _x < width'
                    && p ^. _x > -width') $ _2 .= True

button :: (Monad m, Figure2D m, Mouse m) => BoundingBox Float -> m () -> Karakuri m Bool
button rect m = (\(x, y) -> not x && y) <$> stateful go (False, False) where
    go = do
        p <- mousePosition
        col <- bool (Color 1 0 0 0.7) (Color 1 0.5 0 0.7) `liftM` use _2
        colored col $ polygon $ rect ^.. _Corners
        lift m
        _1 <~ use _2
        _2 <~ return (inBoundingBox p rect) <&=> mouseButtonL
