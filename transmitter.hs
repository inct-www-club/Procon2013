{-# LANGUAGE LambdaCase, BangPatterns #-}
import Encoder.Encoder
import Preview.Preview
import Control.Lens
import Control.Seq
import System.Environment

main = getArgs >>= \case
    ("encodef" : path : _) -> do
        str <- readFile path
        print str
        previewMain False $! str ^. translateAll
    ("encode" : path : _) -> do
        str <- readFile path
        print str
        previewMain True $! str ^. translateAll
    _ -> fail "transmitter encodef | encode"