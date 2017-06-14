module CodeGen where

import AST
import Data.Hex
import Data.List

test :: String
test = "test"


data Address = Adr String deriving (Eq)

instance Show Address where
    show adr@(Adr str@('$':xs)) = str
    show adr@(Adr str) = '$':(replicate (4 - (length str)) '0')++str

instance Ord Address where
    compare a@(Adr numA) b@(Adr numB) = compare (hex (drop 1 numA)) (hex (drop 1 numB))

--data Register = Reg Int deriving (Eq, Show)