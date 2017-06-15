module CodeGen where

import AST hiding (State)
import LanguageData
import Grammar
import Data.List
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe
import Data.Tree
import Data.Text (splitOn, pack, unpack)


type Hex = String

--Conversion helper functions
hexToDec :: Hex -> Int
hexToDec hex = foldl' f 0 hex where
    f n c = 16*n + hexChar c
hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $ 
    elemIndex ch "0123456789ABCDEF"

decToHex :: Int -> Hex
decToHex dec = showIntAtBase 16 intToDigit dec ""


--Types
data State = State (Tree String) [(Var, Loc)] Int String deriving (Eq, Show)
type Var = Char
type Loc = String


--Start



generateCode :: State -> String
generateCode tr = undefined










