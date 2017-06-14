module CodeGen where

import AST
import Data.List
import Data.Hex 
import Data.Maybe




hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $ 
    elemIndex ch "0123456789ABCDEF"

parseHex hex = foldl' f 0 hex where
    f n c = 16*n + hexChar c

data Address = Adr String deriving (Eq)

instance Show Address where
    show adr@(Adr str@('$':xs)) = str
    show adr@(Adr str) = '$':(replicate (4 - (length str)) '0')++str

instance Ord Address where
    compare a@(Adr numA) b@(Adr numB) = compare (parseHex (drop 1 numA)) (parseHex (drop 1 numB))

--data Register =  deriving (Eq, Show)







