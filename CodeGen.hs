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
ldaC :: Hex -> String
ldaC hex@(Hex x) = if (x==0) then "A9 00 "
                   else if (x < 10) then "A9 0"++(dropWhile (=='0') (show hex))++" "
                   else "A9 "++(dropWhile (=='0') (show hex))++" "
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
ldxC :: Hex -> String
ldxC hex@(Hex x) = if (x==0) then "A2 00 "  
                   else if (x < 10) then "A2 0"++(dropWhile (=='0') (show hex))++" "
                   else "A2 "++(dropWhile (=='0') (show hex))++" "
--Load the x register from memory 
ldxM :: Hex -> String
ldxM hex = "AE "++(lEndian hex)
--Load the y register with a constant
ldyC :: Hex -> String
ldyC hex@(Hex x) = if (x==0) then "A0 00 "
                   else if (x < 10) then "A0 0"++(dropWhile (=='0') (show hex))++" " 
                   else "A0 "++(dropWhile (=='0') (show hex))++" "
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

--uses 5 addresses => (nol+5)
storeTrueAt :: Hex -> String
storeTrueAt start = ((ldaC (Hex 84))++(sta start)++
                     (ldaC (Hex 82))++(sta (start+1))++
                     (ldaC (Hex 85))++(sta (start+2))++
                     (ldaC (Hex 69))++(sta (start+3))++
                     (ldaC (Hex 00))++(sta (start+4)))
--uses 6 addresses => (nol+6)
storeFalseAt :: Hex -> String
storeFalseAt start = ((ldaC (Hex 70))++(sta start)++
                      (ldaC (Hex 65))++(sta (start+1))++
                      (ldaC (Hex 76))++(sta (start+2))++
                      (ldaC (Hex 83))++(sta (start+3))++
                      (ldaC (Hex 69))++(sta (start+4))++
                      (ldaC (Hex 00))++(sta (start+5)))

storeWordAt :: String -> Hex -> String -> String
storeWordAt [] _ rtrn = rtrn
storeWordAt (s:ss) start rtrn = storeWordAt ss (start+1) (rtrn++(ldaC (charToHex s))++(sta start))


--Types
data State = State SymbolTable (Forest String) [String] [(Var, Loc)] Int String deriving (Eq, Show)
type Var = String
type Loc = Hex




-----------------------------CODE GEN-----------------------------

generateCode :: (SymbolTable, Tree String) -> String
generateCode (st, tree@(Node val kids)) = genCode (State st kids (drop 1 $ flatten tree) [] 0 [])

genCode :: State -> String

--BASE CASE: NO ELEMENTS LEFT IN FLATTENED TREE
genCode state@(State _ _ [] _ _ toReturn) =  if (((last $ words $ toReturn)) /= "FF") then (toReturn++sys++brk)
                                           else (toReturn++brk)



--VAR DECL
genCode state@(State st (kid@(Node val@("<Variable Declaration>") subKids):kids) flatTree varlocs nextOpenLoc toReturn) = 
    genCode (State st
                   (kids) 
                   (drop (length (flatten kid)) flatTree) 
                   (varlocs++[((getVal (subKids!!1)), (decToHex nextOpenLoc))]) 
                   (nextOpenLoc+1) 
                   toReturn
            )

--ASSIGN STATEMENT
genCode state@(State st (kid@(Node val@("<Assign Statement>") subKids):kids) flatTree varlocs nextOpenLoc toReturn) = 
    genCode (State st
                   (kids)
                   (drop (length (flatten kid)) flatTree)
                   (varlocs)
                   (snd (cgAssign st varlocs nextOpenLoc (flatTree!!1) (flatTree!!2) [] 0 []))
                   (toReturn++(fst (cgAssign st varlocs nextOpenLoc (flatTree!!1) (flatTree!!2) [] 0 [])))
            )

--PRINT STATEMENT
genCode state@(State st (kid@(Node val@("<Print Statement>") subKids):kids) flatTree varlocs nextOpenLoc toReturn) = 
    genCode (State st
                   (kids)
                   (drop (length (flatten kid)) flatTree)
                   (varlocs)
                   (snd (cgPrint st varlocs nextOpenLoc (flatTree!!1) [] 0 []))
                   (toReturn++(fst (cgPrint st varlocs nextOpenLoc (flatTree!!1) [] 0 [])))
            )



--ERROR: pattern not matched
genCode state = error "Pattern not matched in genCode!!!"

--      st             varlocs         nol    cond        block       rtrn      count  dtype     OUTPUT
--cgIf :: SymbolTable -> [(Var, Loc)] -> Int -> [String] -> [String] -> String -> Int -> String -> (String, Int)



--          st             varlocs         nol    lhs       input     rtrn      count  dtype     OUTPUT
cgAssign :: SymbolTable -> [(Var, Loc)] -> Int -> String -> String -> String -> Int -> String -> (String, Int)
cgAssign st varlocs nol lhs []     rtrn count dtype = if (dtype /= "string") then (rtrn, nol)
                                                    else ((rtrn++"00 "), nol)
cgAssign st varlocs nol lhs (i:is) rtrn count dtype =
    --if an ID
    if ( (count == 0) && ((length (i:is) == 1) && (isValidId (i:is))) ) 
        then ( ( rtrn++(ldaM (getLoc varlocs (i:is)))++(sta (getLoc varlocs lhs)) ), nol )
    --if an int literal
    else if ( (dtype == "int") || ((decideType (i:is)) == "int") ) then
        --if first int (no adc)
        if ((count == 0) && (i == '0' || i == '1' || i == '2' || i == '3' || i == '4' || i == '5' || i == '6' || i == '7' || i == '8' || i == '9'))
            then cgAssign st varlocs nol lhs is 
                (rtrn++(ldaC (Hex (digitToInt i)))++
                       (sta (getLoc varlocs lhs))) (count+1) "int"
        --if second or more int (need adc)
        else if ((count > 0) && (i == '0' || i == '1' || i == '2' || i == '3' || i == '4' || i == '5' || i == '6' || i == '7' || i == '8' || i == '9'))
            then cgAssign st varlocs nol lhs is 
                (rtrn++(ldaC (Hex (digitToInt i)))++
                       (adc (getLoc varlocs lhs))++
                       (sta (getLoc varlocs lhs))) (count+1) "int"
        --if second or more var (need adc, can't have a first var)
        else if ((count > 0) && (isValidId (i:[])))
            then cgAssign st varlocs nol lhs is 
                (rtrn++(ldaM (getLoc varlocs (i:[])))++
                       (adc (getLoc varlocs lhs))++
                       (sta (getLoc varlocs lhs))) (count+1) "int"
        --other
        else if (i == '+')
            then cgAssign st varlocs nol lhs is rtrn (count+1) "int"
        else if (i == ',')
            then cgAssign st varlocs nol lhs is rtrn (count+1) dtype
        else error "not yet reached!!! (but reached int in assign)"
    --if string literal
    else if ( (dtype == "string") || ((decideType (i:is)) == "string") ) then
        ((rtrn++(storeWordAt (i:is) (Hex nol) [])++
                (ldaC (Hex nol))++
                (sta (getLoc varlocs lhs))), (nol+(length (i:is))))
    --if boolean literal
    else if ( (dtype == "boolean") || ((decideType (i:is)) == "boolean") ) then
        --if (i:is) true
        if ((i:is) == "true") 
            then ((rtrn++(storeTrueAt (Hex nol))++
                         (ldaC (Hex nol))++
                         (sta (getLoc varlocs lhs))), (nol+5))
        --if (i:is) false
        else if ((i:is) == "false") 
            then ((rtrn++(storeFalseAt (Hex nol))++
                         (ldaC (Hex nol))++
                         (sta (getLoc varlocs lhs))), (nol+6))
        --condition for booleanExpr Mults HERE!!!
        --else if
        else error "not yet reached!!! (but reached boolean in assign)"
    else error "not yet reached!!!"
    --where

--         st             varlocs         nol    input     rtrn      count  dtype     OUTPUT
cgPrint :: SymbolTable -> [(Var, Loc)] -> Int -> String -> String -> Int -> String -> (String, Int)
--BASE CASE: always reached, unlike cgAssign's ID condition
cgPrint st varlocs nol [] rtrn count dtype = 
    
    if (dtype == "int") then
        ( (rtrn++
        (sta (Hex nol))++
        (ldyM (Hex nol))++
        (ldxC (Hex 1))++
        sys), (nol+1) )
    else 
        (rtrn, nol)

    --else error "cgPrint pattern not reached!!!"

cgPrint st varlocs nol (i:is) rtrn count dtype =
    --i is an ID and i is first
    if ( (count == 0) && ((length (i:is) == 1) && (isValidId (i:is))) ) then 
        --id is int
        if((getType (i:is) st) == "int")  
            then cgPrint st varlocs nol is (rtrn++(ldaM (getLoc varlocs (i:is)))) (count+1) "int"
        --id is string
        else if ((getType (i:is) st) == "string")
            then cgPrint st varlocs (nol) [] (rtrn++
                                             (ldyM (getLoc varlocs (i:is)))++
                                             (ldxC (Hex 2))++
                                             sys) (count+1) "string"

        --id is boolean (check if 0 or 1 using op codes) (always nol+6 incase false)
        else if ((getType (i:is) st) == "boolean") 
            then cgPrint st varlocs (nol) [] (rtrn++
                                             (ldyM (getLoc varlocs (i:is)))++
                                             (ldxC (Hex 2))++
                                             sys) (count+1) "boolean"
            

        else error "not yet reached!!! (but reached id in assign)"
    --i is an int literal 
    else if ( (dtype == "int") || ((decideType (i:is)) == "int") ) then 
        --i is a first int
        if ((count == 0) && (iIsInt i))
            then cgPrint st varlocs nol is
                            (rtrn++(ldaC (Hex (digitToInt i))))       (count+1) "int"
        --i is a second or more int
        else if ((count > 0) && (iIsInt i)) 
            then cgPrint st varlocs nol is
                            (rtrn++(adc $ Hex $ digitToInt i))       (count+1) "int"
        --i is a second or more var
        else if ((count > 0) && (isValidId (i:[])))
            then cgPrint st varlocs (nol+1) is
                             (rtrn++(sta (Hex nol))++
                                    (ldaM (getLoc varlocs (i:[])))++
                                    (adc (Hex nol)))      (count+1) "int"
        --other
        else if (i == '+')
            then cgPrint st varlocs nol is rtrn (count+1) "int"
        else if (i == ',')
            then cgPrint st varlocs nol is rtrn (count+1) dtype
        else error "not yet reached!!! (but reached int in assign)"
    --i is a string literal
    else if ( (dtype == "string") || ((decideType (i:is)) == "string") ) then
        cgPrint st varlocs (nol+(length (i:is))) [] 
            (rtrn++(storeWordAt (i:is) (Hex nol) [])++(ldyC (Hex nol))++(ldxC (Hex 2))++sys) (count+1) "string"
    --i is a boolean literal
    else if ( (dtype == "boolean") || ((decideType (i:is)) == "boolean") ) then
        --(i:is) is true
        if ((i:is) == "true")
            then cgPrint st varlocs (nol+5) [] 
                            (rtrn++(storeTrueAt  (Hex nol))++(ldyC (Hex nol))++(ldxC (Hex 2))++sys) (count+1) "boolean"
        --(i:is) is false
        else if ((i:is) == "false")
            then cgPrint st varlocs (nol+6) [] 
                            (rtrn++(storeFalseAt (Hex nol))++(ldyC (Hex nol))++(ldxC (Hex 2))++sys) (count+1) "boolean"
        --(i:is) is boolExp Mult [CONDITION GOES HERE]
        --
        else error "not yet reached!!! (but reached boolean in assign)"

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

charToHex :: Char -> Hex
charToHex chr@('a') = Hex 65
charToHex chr@('b') = Hex 66
charToHex chr@('c') = Hex 67
charToHex chr@('d') = Hex 68
charToHex chr@('e') = Hex 69
charToHex chr@('f') = Hex 70
charToHex chr@('g') = Hex 71
charToHex chr@('h') = Hex 72
charToHex chr@('i') = Hex 73
charToHex chr@('j') = Hex 74
charToHex chr@('k') = Hex 75
charToHex chr@('l') = Hex 76
charToHex chr@('m') = Hex 77
charToHex chr@('n') = Hex 78
charToHex chr@('o') = Hex 79
charToHex chr@('p') = Hex 80
charToHex chr@('q') = Hex 81
charToHex chr@('r') = Hex 82
charToHex chr@('s') = Hex 83
charToHex chr@('t') = Hex 84
charToHex chr@('u') = Hex 85
charToHex chr@('v') = Hex 86
charToHex chr@('w') = Hex 87
charToHex chr@('x') = Hex 88
charToHex chr@('y') = Hex 89
charToHex chr@('z') = Hex 90
charToHex chr@(' ') = Hex 32

