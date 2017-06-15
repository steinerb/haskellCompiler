module CodeGen where

import AST hiding (State, decideType, decideInt, decideString)
import LanguageData
import Grammar
import Data.List
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit, toUpper)
import Data.Maybe
import Data.Tree
import Data.Text (splitOn, pack, unpack)

--type Hex = String

data Hex = Hex Int deriving (Eq)
--show handles Int-> Hex conversion and missing zeros. conversion functions support a prefix of zeros.
--NOT little endian!
instance Show Hex where
    show hex@(Hex innerInt) = 
         (replicate (4 - (length (decToHexStr $ strToInt $ (strInt)))) '0')++(decToHexStr $ strToInt $ (strInt))
        where strInt = show innerInt

instance Num Hex where
    hex1@(Hex int1) + hex2@(Hex int2) = Hex (int1+int2)
    hex1@(Hex int1) * hex2@(Hex int2) = Hex (int1*int2)
    hex1@(Hex int1) - hex2@(Hex int2) = Hex (int1-int2)
    fromInteger int = Hex (fromInteger int)
    negate hex@(Hex int) = Hex (negate int)
    abs hex@(Hex int) = if (int < 0) then Hex (negate int) else Hex int
    signum hex@(Hex int) = if (int < 0) then -1 else if (int == 0) then 0 else 1


--Conversion helper functions
hexStrToDec :: String -> Int
hexStrToDec hex = foldl' f 0 hex where
    f n c = 16*n + hexChar c
hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $ 
    elemIndex ch "0123456789ABCDEF"

decToHexStr :: Int -> String
decToHexStr dec = map (toUpper) (showIntAtBase 16 intToDigit dec "")

decToHex :: Int -> Hex
decToHex dec = Hex dec

toInteger :: Int -> Integer
toInteger int = read (show int) :: Integer

toInt :: Integer -> Int 
toInt int = read (show int) :: Int


--Hex Formatting helper functions
--little-endian
lEndian :: Hex -> String
lEndian hex = (drop 2 $ show hex)++" "++(take 2 $ show hex)

--Load the accumulator with a constant
ldaC :: Int -> String
ldaC const = if (const < 10) then "A9 0"++(show const) else "A9 "++(show const)
--Load the accumulator from memory 
ldaM :: Hex -> String
ldaM hex = "AD "++(lEndian hex)
--Store the accumulator in memory
sta :: Hex -> String
sta hex = "8D "++(lEndian hex)
--Add with carry ( Adds contents of an address to the contents of the accumulator and keeps the result in the accumulator)
adc :: Hex -> String
adc hex = "6D "++(lEndian hex)
--Load the x register with a constant
ldxC :: Int -> String
ldaC const = if (const < 10) then "A2 0"++(show const) else "A2 "++(show const)
--Load the x register from memory 
ldxM :: Hex -> String
ldaM hex = "AE "++(lEndian hex)
--Load the y register with a constant
ldyC :: Int -> String
ldaC const = if (const < 10) then "A0 0"++(show const) else "A0 "++(show const)
--Load the y register from memory 
ldyM :: Hex -> String
ldaM hex = "AC "++(lEndian hex)



--Types
data State = State (Forest String) [String] [(Var, Loc)] Int String deriving (Eq, Show)
type Var = String
type Loc = Int


--Start



generateCode :: Tree String -> String
generateCode tree@(Node val kids) = genCode (State kids (drop 1 $ flatten tree) [] 0 [])

genCode :: State -> String

--BASE CASE: NO ELEMENTS LEFT IN FLATTENED TREE
genCode state@(State _ [] _ _ toReturn) = toReturn

--VAR DECL
genCode state@(State (kid@(Node val@("<Variable Declaration>") subKids):kids) flatTree varlocs nextOpenLoc toReturn) = 
    genCode (State (kids) 
                   (drop (length (flatten kid)) flatTree) 
                   (varlocs++[((getVal (subKids!!1)), nextOpenLoc)]) 
                   (nextOpenLoc+1) 
                   toReturn
            )

--ASSIGN STATEMENT
genCode state@(State (kid@(Node val@("<Assign Statement>") subKids):kids) flatTree varlocs nextOpenLoc toReturn) = 
    genCode (State (kids)
                   (drop (length (flatten kid)) flatTree)
                   (varlocs)
                   (snd (cgAssign varlocs nextOpenLoc (flatTree!!2) [] 0))
                   (toReturn++(fst (cgAssign varlocs nextOpenLoc (flatTree!!2) [] 0)))
            )



--ERROR: pattern not matched

genCode state = error "Pattern not matched in genCode!!!"


cgAssign :: [(Var, Loc)] -> Int -> String -> String -> Int -> (String, Int)
cgAssign varlocs nol [] rtrn count = (rtrn, nol)
cgAssign varlocs nol input rtrn count = undefined
    --if only been through the loop once and length = 1
--    if ( (count == 0) && ((length input == 1) && (isValidId input)) ) 
--        then (rtrn++()      ,...)

    --if only been through the loop multiple times and length = 1

    --if ( (decideType input) == "int") then cgInt
    --where







decideType :: String -> String
decideType a =
         if ((decideInt a) == True) then "int"
    else if ((decideString a) == True) then "string"
    else "boolean"
--CONDITION: MUST RUN BEFORE decideString TO PICK UP NUMBERS BEFORE LENGTH IS COUNTED!!!
decideInt :: String -> Bool
decideInt a = 
    if ((True `notElem` (map (=='=') a)) && (("0" `isInfixOf` a) || ("1" `isInfixOf` a) || ("2" `isInfixOf` a) || ("3" `isInfixOf` a) || ("4" `isInfixOf` a) || ("5" `isInfixOf` a) || ("6" `isInfixOf` a) || ("7" `isInfixOf` a) || ("8" `isInfixOf` a) || ("9" `isInfixOf` a)) )
        then True 
    else False

decideString :: String -> Bool
decideString a = 
    if ( ((length a) > 1) && (a /= "true") && (a /= "false") && (a /= "NOTSAME") && (True `notElem` (map (=='=') a)) && (True `notElem` (map (=='<') a)) ) 
        then True
    else False



getVar :: [(Var, Loc)] -> Loc -> Var
getVar varlocs loc = fst $ head (filter ((==loc).snd) varlocs)

getLoc :: [(Var, Loc)] -> Var -> Loc
getLoc varlocs var = snd $ head (filter ((==var).fst) varlocs)

strToInt :: String -> Int
strToInt str = read str :: Int