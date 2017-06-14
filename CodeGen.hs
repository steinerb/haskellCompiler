module CodeGen where

import AST
import Data.List
import Data.Hex (hex, unhex)
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe



--Conversion helper functions
hexToDec :: Hex -> Int
hexToDec hex = foldl' f 0 hex where
    f n c = 16*n + hexChar c
hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $ 
    elemIndex ch "0123456789ABCDEF"

decToHex :: Int -> Hex
decToHex dec = showIntAtBase 16 intToDigit dec ""

--Types
type Hex = String

type Loc = Hex
type Val = Hex

data Address = Adr {loc :: Loc, val :: Val} deriving (Eq)

data Register = Reg Val deriving (Eq, Show)

data Memory = Memory {addresses :: [Address], xReg :: Register, yReg :: Register, acc :: Register} deriving (Eq, Show)

instance Show Address where
    show adr@(Adr str@('$':xs) val) = str++":    "++val
    show adr@(Adr str val) = '$':(replicate (4 - (length str)) '0')++str++":    "++val

instance Ord Address where
    compare a@(Adr numA _) b@(Adr numB _) = compare (hexToDec (drop 1 numA)) (hexToDec (drop 1 numB))

getLoc :: Address -> Hex
getLoc adr@(Adr str _) = (replicate (4 - (length str)) '0')++(loc adr)







