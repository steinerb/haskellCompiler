module CodeGen where

import AST hiding (State, decideType, decideInt, decideString)
import LanguageData
import Grammar
import Data.List
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit, toUpper, digitToInt)
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
lEndian hex = (drop 2 $ show hex)++" "++(take 2 $ show hex)++" "

--Load the accumulator with a constant
ldaC :: Int -> String
ldaC const = if (const < 10) then "A9 0"++(show const)++" " else "A9 "++(show const)++" "
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
ldxC const = if (const < 10) then "A2 0"++(show const)++" " else "A2 "++(show const)++" "
--Load the x register from memory 
ldxM :: Hex -> String
ldxM hex = "AE "++(lEndian hex)
--Load the y register with a constant
ldyC :: Int -> String
ldyC const = if (const < 10) then "A0 0"++(show const)++" " else "A0 "++(show const)++" "
--Load the y register from memory 
ldyM :: Hex -> String
ldyM hex = "AC "++(lEndian hex)
--No Operation
nop :: String
nop = "EA "
--Break (which is really a system call)
brk :: String
brk = "00 "
--Compare a byte in memory to the X reg. Set the Z (zero) flag if equal
cpx :: Hex -> String
cpx hex = "EC "++(lEndian hex)
--Branch nbytes if Z flag = 0
bne :: Hex -> String
bne hex = "D0 "++(take 2 (lEndian hex))
--Increment the value of a byte    
inc :: Hex -> String
inc hex = "EE "++(lEndian hex)
--System Call
sys :: String
sys = "FF "
--Constant #$01 in X reg = print integer in Y reg
--Constant #$02 in X reg = print the 00-terminated string stored at address in Y reg.



--Types
data State = State (Forest String) [String] [(Var, Loc)] Int String deriving (Eq, Show)
type Var = String
type Loc = Hex




-----------------------------CODE GEN-----------------------------

generateCode :: Tree String -> String
generateCode tree@(Node val kids) = genCode (State kids (drop 1 $ flatten tree) [] 0 [])

genCode :: State -> String

--BASE CASE: NO ELEMENTS LEFT IN FLATTENED TREE
genCode state@(State _ [] _ _ toReturn) =  if (((last $ words $ toReturn)) /= "FF") then (toReturn++sys++brk)
                                           else (toReturn++brk)


--VAR DECL
genCode state@(State (kid@(Node val@("<Variable Declaration>") subKids):kids) flatTree varlocs nextOpenLoc toReturn) = 
    genCode (State (kids) 
                   (drop (length (flatten kid)) flatTree) 
                   (varlocs++[((getVal (subKids!!1)), (decToHex nextOpenLoc))]) 
                   (nextOpenLoc+1) 
                   toReturn
            )

--ASSIGN STATEMENT
genCode state@(State (kid@(Node val@("<Assign Statement>") subKids):kids) flatTree varlocs nextOpenLoc toReturn) = 
    genCode (State (kids)
                   (drop (length (flatten kid)) flatTree)
                   (varlocs)
                   (snd (cgAssign varlocs nextOpenLoc (flatTree!!1) (flatTree!!2) [] 0 []))
                   (toReturn++(fst (cgAssign varlocs nextOpenLoc (flatTree!!1) (flatTree!!2) [] 0 [])))
            )

--PRINT STATEMENT
genCode state@(State (kid@(Node val@("<Print Statement>") subKids):kids) flatTree varlocs nextOpenLoc toReturn) = 
    genCode (State (kids)
                   (drop (length (flatten kid)) flatTree)
                   (varlocs)
                   (snd (cgPrint varlocs nextOpenLoc (flatTree!!1) [] 0 []))
                   (toReturn++(fst (cgPrint varlocs nextOpenLoc (flatTree!!1) [] 0 [])))
            )


--ERROR: pattern not matched
genCode state = error "Pattern not matched in genCode!!!"

--          varlocs         nol    lhs       input     rtrn      count  dtype     OUTPUT
cgAssign :: [(Var, Loc)] -> Int -> String -> String -> String -> Int -> String -> (String, Int)
cgAssign varlocs nol lhs []     rtrn count dtype = (rtrn, nol)
cgAssign varlocs nol lhs (i:is) rtrn count dtype =
    --if an ID
    if ( (count == 0) && ((length (i:is) == 1) && (isValidId (i:is))) ) 
        then ( ( rtrn++(ldaM (getLoc varlocs (i:is)))++(sta (getLoc varlocs lhs)) ), nol )
    --if an int literal
    else if ( (dtype == "int") || ((decideType (i:is)) == "int") ) then
        --if first int (no adc)
        if ((count == 0) && (i == '0' || i == '1' || i == '2' || i == '3' || i == '4' || i == '5' || i == '6' || i == '7' || i == '8' || i == '9'))
            then cgAssign varlocs nol lhs is 
                (rtrn++(ldaC (digitToInt i))++
                       (sta (getLoc varlocs lhs))) (count+1) "int"
        --if second or more int (need adc)
        else if ((count > 0) && (i == '0' || i == '1' || i == '2' || i == '3' || i == '4' || i == '5' || i == '6' || i == '7' || i == '8' || i == '9'))
            then cgAssign varlocs nol lhs is 
                (rtrn++(ldaC (digitToInt i))++
                       (adc (getLoc varlocs lhs))++
                       (sta (getLoc varlocs lhs))) (count+1) "int"
        --if second or more var (need adc, can't have a first var)
        else if ((count > 0) && (isValidId (i:[])))
            then cgAssign varlocs nol lhs is 
                (rtrn++(ldaM (getLoc varlocs (i:[])))++
                       (adc (getLoc varlocs lhs))++
                       (sta (getLoc varlocs lhs))) (count+1) "int"
        --other
        else if (i == '+')
            then cgAssign varlocs nol lhs is rtrn (count+1) "int"
        else if (i == ',')
            then cgAssign varlocs nol lhs is rtrn (count+1) dtype
        else error "not yet reached!!! (but reached int in assign)"

    --if string literal
    --else if ( (dtype == "string") || ((decideType (i:is)) == "string") )


    else error "not yet reached!!!"
    --where

--         varlocs         nol    input     rtrn      count  dtype     OUTPUT
cgPrint :: [(Var, Loc)] -> Int -> String -> String -> Int -> String -> (String, Int)
cgPrint varlocs nol [] rtrn count dtype = ( (rtrn++
                                            (sta (Hex nol))++
                                            (ldyM (Hex nol))++
                                            (ldxC 1)++
                                            sys), (nol+1) ) 
cgPrint varlocs nol (i:is) rtrn count dtype =
    --i is an ID and i is first
    --if ( (count == 0) && ((length (i:is) == 1) && (isValidId (i:is))) ) 
    --    then ( rtrn++(ldaM (getLoc varlocs (i:is))) )

    --i is an int literal 
    if ( (dtype == "int") || ((decideType (i:is)) == "int") ) then 
        --i is a first int
        if ((count == 0) && (iIsInt i))
            then cgPrint varlocs nol is
                            (rtrn++(ldaC (digitToInt i)))       (count+1) "int"
        --i is a second or more int
        else if ((count > 0) && (iIsInt i)) 
            then cgPrint varlocs nol is
                            (rtrn++(ldaC (digitToInt i)))       (count+1) "int"
        --i is a second or more var
        else if ((count > 0) && (isValidId (i:[])))
            then cgPrint varlocs nol is
                            (rtrn++(ldaM (getLoc varlocs (i:[])))++
                                    (adc (getLoc varlocs (i:[]))))      (count+1) "int"

        --other
        else if (i == '+')
            then cgPrint varlocs nol is rtrn (count+1) "int"
        else if (i == ',')
            then cgPrint varlocs nol is rtrn (count+1) dtype
        else error "not yet reached!!! (but reached int in assign)"

    else error "not yet reached!!!"
    where
        iIsInt i = if (i == '0' || i == '1' || i == '2' || i == '3' || i == '4' || i == '5' || i == '6' || i == '7' || i == '8' || i == '9') then True else False




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

charToInt :: Char -> Int
charToInt chr = strToInt ("\""++[chr]++"\"")

