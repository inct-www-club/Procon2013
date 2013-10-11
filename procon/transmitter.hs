{-# LANGUAGE LambdaCase, BangPatterns #-}
import Encoding
import Preview
import Control.Lens
import Control.Seq
import System.Environment
import Data.Char

main = getArgs >>= \case
    ("encodef" : path : _) -> do
        str <- readFile path
        previewMain False $! str ^. transcode . to (map intToDigit)
    ("encode" : path : _) -> do
        str <- readFile path
        previewMain True $! str ^. transcode . to (map intToDigit)
    _ -> fail "transmitter [encodef | encode] [path]"