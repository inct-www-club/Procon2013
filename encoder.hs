import Control.Applicative
import Data.List
import Control.Lens
import Control.Monad.State
import System.Environment

letters :: [Char]
letters = concat $ f <$> [2..7] <*> [0..15] where
    f x y = [toEnum v | let v = x * 16 + y, v `notElem` [0x20, 0x5B, 0x5C, 0x5D, 0x5E, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F]]

_Letter :: Iso' Char Int
_Letter = iso (\x -> elemIndex x letters ^?! _Just) (letters !!)

data Bit = Black | Red | White deriving (Show, Eq, Ord, Enum)

_Stream :: Iso' [Char] [Bit]
_Stream = encodeString . encodeBlocks

data Block = Triad Int Int Int | Pair Int Int | Unit Int deriving (Show, Eq, Ord)

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

encodeBlocks :: Iso' [Block] [Bit]
encodeBlocks = iso encode decode where
    encode (b : bs) = case b ^? encodeBlock of
        Just r -> r ++ encode bs
        Nothing -> encode (recons $ b : bs)
    encode [] = []
    decode bs
        | length bs >= 9 = (review encodeBlock $ take 9 bs) : decode (drop 9 bs)
        | length bs == 0 = []
        | otherwise = error "extra bits"

consBlock :: Int -> [Block] -> [Block]
consBlock x [] = [Unit x]
consBlock x (Unit y:ts) = Pair x y : ts
consBlock x (Pair y z:zs) = Triad x y z : zs
consBlock x (Triad y z w : ws) = Triad x y z : consBlock w ws

recons (Triad x y z : xs) = Pair x y : consBlock z xs
recons xs = xs

-- TODO: Encoding with genetic algorithm

encodeBlock :: Prism' Block [Bit]
encodeBlock = prism' decode encode where
    encode (Triad x y z) = (x * 86 ^ 2 + y * 86 + z + 86 ^ 2 - 1) ^? _Int_Bit
    encode (Pair x y) = ((x + 1) * 86 + y) ^? _Int_Bit
    encode (Unit x) = x ^? _Int_Bit
    decode bs
        | n < 86 = Unit n
        | otherwise = Pair (div n 86 - 1) (mod n 86)
        where
            n = review _Int_Bit bs

_Int_Bit :: Prism' Int [Bit]
_Int_Bit = prism' (foldr (\x r -> r * 3 + fromEnum x) 0) (enc 9) where
    enc m n
        | m < 0 = Nothing
        | n == 0 = Just (replicate m Black)
        | otherwise = (toEnum (mod n 3):) <$> enc (m - 1) (div n 3)

_ReprBits :: Iso' [Bit] String
_ReprBits = iso encode decode where
    encode (Red : xs) = 'R' : encode xs
    encode (Black : xs) = 'B' : encode xs
    encode (White : xs) = 'W' : encode xs
    encode [] = []
    decode ('R' : xs) = Red : decode xs
    decode ('W' : xs) = White : decode xs
    decode ('B' : xs) = Black : decode xs
    decode (_ : xs) = error "Illegal character: expecting R, W, B"
    decode [] = []

_Lines :: Iso' String [String]
_Lines = iso lines unlines

main = do
    args <- getArgs
    let f = interact . over _Lines . view . mapping
    case args of
         ("encode":_) -> f (_Stream . _ReprBits)
         ("decode":_) -> f (from (_Stream . _ReprBits))
         _ -> putStrLn "Usage: encoder [encode|decode]"