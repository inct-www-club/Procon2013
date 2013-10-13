module Encoding where
import Control.Lens
import Control.Applicative
import Data.List
import Debug.Trace

-- | The list of letters
letters :: [Char]
letters = concat $ f <$> [2..7] <*> [0..15] where
    f x y = [toEnum v | let v = x * 16 + y, v `notElem` [0x20, 0x5B, 0x5C, 0x5D, 0x5E, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F]]

numLetters :: Int
numLetters = 86

-- | Convert a character to an integer
_Letter :: Prism' Char Int
_Letter = prism' (letters !!) (flip elemIndex letters)

newtype Die = Die { getDie :: Int } deriving Show

transcode :: Iso' String [Int]
transcode = iso encode decode where
    encode (a : b : xs) = map (+1) [d0, d1, d2, d3, d4] ++ encode xs where
        z0 = b ^?! _Letter * numLetters + a ^?! _Letter
        (z1, d0) = divMod z0 6
        (z2, d1) = divMod z1 6
        (z3, d2) = divMod z2 6
        (z4, d3) = divMod z3 6
        (_, d4) = divMod z4 6
    encode [a] = map (+1) [d0, d1, d2, d3, d4] where
        z0 = numLetters * numLetters + a ^?! _Letter
        (z1, d0) = divMod z0 6
        (z2, d1) = divMod z1 6
        (z3, d2) = divMod z2 6
        (z4, d3) = divMod z3 6
        (_, d4) = divMod z4 6
    encode [] = []
    decode (d0:d1 :d2 :d3 :d4 :xs) = _Letter # a : [_Letter # b| b < numLetters] ++ decode xs where
        (b, a) = divMod (sum $ zipWith (*) (iterate (*6) 1) $ [d0 - 1, d1 - 1, d2 - 1, d3 - 1, d4 - 1]) numLetters
    decode _ = []