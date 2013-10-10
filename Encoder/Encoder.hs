module Encoder.Encoder (translateAll) where

import Control.Applicative
import Data.List
import Control.Lens
import Control.Monad.State
import System.Environment

translateAll :: Iso' String String
translateAll = _Stream . mapping _ReprDie

-- | The list of letters
letters :: [Char]
letters = concat $ f <$> [2..7] <*> [0..15] where
    f x y = [toEnum v | let v = x * 16 + y, v `notElem` [0x20, 0x5B, 0x5C, 0x5D, 0x5E, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F]]

-- | Convert a character to an integer
_Letter :: Iso' Char Int
_Letter = iso (\x -> elemIndex x letters ^?! _Just) (letters !!)

-- | The type of dice
data Die = D5 | D1 | D2 deriving (Show, Eq, Ord, Enum)

-- | Convert a string to dice
_Stream :: Iso' [Char] [Die]
_Stream = encodeString . encodeBlocks

data Block = Triad Int Int Int | Pair Int Int | Unit Int deriving (Show, Eq, Ord)

-- | Convert a string to blocks
encodeString :: Iso' [Char] [Block]
encodeString = iso toBlocks fromBlocks where
    toBlocks :: [Char] -> [Block]
    toBlocks (x:y:z:zs) = Triad (view _Letter x) (view _Letter y) (view _Letter z) : toBlocks zs
    toBlocks (x:y:ys) = Pair (view _Letter x) (view _Letter y) : toBlocks ys
    toBlocks (x:xs) = Unit (view _Letter x) : toBlocks xs
    toBlocks [] = []

    fromBlocks :: [Block] -> [Char]
    fromBlocks (Unit x : bs) = view (from _Letter) x : fromBlocks bs
    fromBlocks (Pair x y : bs) = view (from _Letter) x : view (from _Letter) y : fromBlocks bs
    fromBlocks (Triad x y z : bs) = view (from _Letter) x : view (from _Letter) y : view (from _Letter) z : fromBlocks bs
    fromBlocks [] = []

-- | Convert blocks to dice
encodeBlocks :: Iso' [Block] [Die]
encodeBlocks = iso encode decode where
    encode (b : bs) = case b ^? encodeBlock of
        Just r -> r ++ encode bs
        Nothing -> encode (recons $ b : bs)
    encode [] = []
    decode bs
        | length bs >= 9 = (review encodeBlock $ take 9 bs) : decode (drop 9 bs)
        | length bs == 0 = []
        | otherwise = error "extra bits"

-- | Prepend an element to blocks
consBlock :: Int -> [Block] -> [Block]
consBlock x [] = [Unit x]
consBlock x (Unit y:ts) = Pair x y : ts
consBlock x (Pair y z:zs) = Triad x y z : zs
consBlock x (Triad y z w : ws) = Triad x y z : consBlock w ws

-- | Remove triads in the head
recons (Triad x y z : xs) = Pair x y : consBlock z xs
recons xs = xs

-- | Convert blocks to dice
encodeBlock :: Prism' Block [Die]
encodeBlock = prism' decode encode where
    encode (Triad x y z) = (x * 86 ^ 2 + y * 86 + z + 86 ^ 2 - 1) ^? _Int_Die
    encode (Pair x y) = ((x + 1) * 86 + y) ^? _Int_Die
    encode (Unit x) = x ^? _Int_Die
    decode bs
        | n < 86 = Unit n
        | otherwise = Pair (div n 86 - 1) (mod n 86)
        where
            n = review _Int_Die bs

-- | Convert an integer to dice
_Int_Die :: Prism' Int [Die]
_Int_Die = prism' (foldr (\x r -> r * 3 + fromEnum x) 0) (enc 9) where
    enc m n
        | m < 0 = Nothing
        | n == 0 = Just (replicate m D5)
        | otherwise = (toEnum (mod n 3):) <$> enc (m - 1) (div n 3)

-- | Representation of dice
_ReprDie :: Iso' Die Char
_ReprDie = iso encode decode where
    encode D1 = '1'
    encode D5 = '2'
    encode D2 = '5'
    decode '1' = D1
    decode '2' = D2
    decode '5' = D5
    decode _ = error "Illegal character: expecting 1, 2, 5"

_Lines :: Iso' String [String]
_Lines = iso lines unlines

main = do
    args <- getArgs
    let f = over _Lines . view . mapping
    case args of
         ("encode":path:_) -> readFile path >>= putStrLn . f (_Stream . mapping _ReprDie) . filter (`elem`letters)
         ("encode":_) -> interact $ f (_Stream . mapping _ReprDie)
         ("decode":_) -> interact $ f (from (_Stream . mapping _ReprDie))
         _ -> putStrLn "Usage: encoder [encode|decode]"