module CodeGen where

import AST
import Data.List
import Data.Hex (hex, unhex)
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe


testMemory :: Memory
testMemory = (Memory [(Adr "12" "1"), (Adr "12" "ZZ")] (Reg "ZZ") (Reg "ZZ") (Reg "ZZ"))

--Conversion helper functions
hexToDec :: Hex -> Int
hexToDec hex = foldl' f 0 hex where
    f n c = 16*n + hexChar c
hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $ 
    elemIndex ch "0123456789ABCDEF"

decToHex :: Int -> Hex
decToHex dec = showIntAtBase 16 intToDigit dec ""

type Hex = String
















