{-# LANGUAGE LambdaCase#-}
import Graphics.UI.FreeGame

import qualified Data.Array.Repa as R

newtype Pattern = Pattern (R.Array U DIM2 Double)

score :: Pattern -> R.Array U DIM2 Word8 -> Double
score (Pattern pat) bmp = let (sh, f) = R.toFuncton bmp
    in R.fromFunction sh (\i@(Z :. y :. x) -> (pat R.! i - f (Z :. k * fromIntegral y :. k * fromIntegral x)) ^ 2)

main = getArgs >>= \case args of
    ()runGame def $ do
