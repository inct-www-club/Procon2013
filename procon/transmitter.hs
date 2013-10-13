{-# LANGUAGE LambdaCase, BangPatterns #-}
import Encoding
import OldEncoding
import Preview
import Control.Lens
import Control.Seq
import System.Environment
import Data.Char
import Control.Exception


main = getArgs >>= \case
    ("encodef" : path : _) -> do
        str <- readFile path
        let p = str ^. transcode . to (map intToDigit)
        print p
        previewMain ((*2) . (`div`5)) False $! p
    ("encode" : path : _) -> do
        str <- readFile path
        let p = str ^. transcode . to (map intToDigit)
        print p
        previewMain ((*2) . (`div`5)) True $! p
    ("oldencodef" : path : _) -> do
        str <- readFile path
        let p = str ^. oldTranscode . to (map intToDigit)
        print p
        previewMain ((`div`73) . (*18)) False $! p
    ("oldencode" : path : _) -> do
        str <- readFile path
        let p = str ^. oldTranscode . to (map intToDigit)
        print p
        previewMain ((`div`73) . (*18)) True $! p
    (path : _) -> do
        str <- readFile path
        let p = str ^. transcode . to (map intToDigit)
        print p
        previewMain ((*2) . (`div`5)) True $! p
    _ -> fail "transmitter [encodef | encode] [path]"