module CodeGen where

import AST
import Data.List
import Data.Hex (hex, unhex)
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe




hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $ 
    elemIndex ch "0123456789ABCDEF"

hexToDec hex = foldl' f 0 hex where
    f n c = 16*n + hexChar c

decToHex dec = showIntAtBase 16 intToDigit dec ""

type Hex = String

data Address = Adr Hex deriving (Eq)

instance Show Address where
    show adr@(Adr str@('$':xs)) = str
    show adr@(Adr str) = '$':(replicate (4 - (length str)) '0')++str

instance Ord Address where
    compare a@(Adr numA) b@(Adr numB) = compare (hexToDec (drop 1 numA)) (hexToDec (drop 1 numB))

data Register = Reg Hex







