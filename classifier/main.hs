{-# LANGUAGE LambdaCase, TemplateHaskell #-}
import Graphics.UI.FreeGame
import Data.Karakuri
import Control.Lens
import Control.Monad.State
import Control.Monad.Free
import System.Environment
import Control.Comonad
import Butai
import Classifier hiding (translate)
import qualified Classifier
import Data.Array.Unboxed as A

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

slider :: (Monad m, Figure2D m, Mouse m) => Float -> Karakuri m Float
slider width = fmap (/width')$ stateful go 0 where
    width' = width / 2
    go = do
        p <- mousePosition
        q <- get
        let color = yellow & _Alpha .~ 0.5
        
        colored color $ do
            translate (V2 q 0) $ polygonOutline [V2 0 0, V2 (-16) (-16), V2 16 (-16)]
            line [V2 (-width') (-16), V2 (-width') 0, V2 width' 0, V2 width' (-16)]
        whenM mouseButtonL $ when (abs (p ^. _y) < 16 && p ^. _x < width' && p ^. _x > -width') $ do
            put (p ^. _x)

button :: (Monad m, Figure2D m, Mouse m) => Karakuri m Bool
button = (\(x, y) -> not x && y) <$> stateful go (False, False) where
    rect = BoundingBox (-16) (-16) 16 16
    go = do
        p <- mousePosition
        col <- bool red yellow `liftM` mouseButtonL
        colored (col & _Alpha .~ 0.5) $ polygon $ rect ^.. _Corners
        _1 <~ use _2
        _2 <~ return (inBoundingBox p rect) <&=> mouseButtonL

data World = World
    { _result :: [V2 Float]
    }
makeLenses ''World

main = getArgs >>= \case
    (path : _) -> runGame def $ runButaiT $ flip evalStateT (World []) $ do
        font <- loadFont "../preview/VL-PGothic-Regular.ttf"
        ks <- sequence
            [register $ draggablePoint (V2 200 200)
            , register $ draggablePoint (V2 280 200)
            , register $ draggablePoint (V2 280 280)
            , register $ draggablePoint (V2 200 280)
            ]
        sl <- register $ transKarakuri (translate (V2 80 360)) (slider 120)
        btn <- register $ transKarakuri (translate (V2 80 80)) button
        bmp <- loadBitmapFromFile path
        let (imgW, imgH) = bitmapSize bmp & each %~ fromIntegral
        forever $ do
            translate (V2 (imgW / 2) (imgH / 2)) $ fromBitmap bmp
            updateAll
            ps <- mapM look ks
            colored red $ polygonOutline ps

            whenM (look btn) $ do
                let [p, q, s, r] = ps
                    quad = Quadrangle p q r s
                    img = integrate $ crop quad (V2 60 60) (bmp ^. _BitmapArray)
                    vs = match img (blackPoints 12 (V2 60 60))
                    cs = map (review (projecting quad)) vs
                result .= cs
            
            vs <- use result
            forM_ vs $ \v -> translate v $ colored (yellow & _Alpha .~ 0.5) $ circle 8
            lift $ lift tick