module Lexer where

import LanguageData
import Data.List


data BoolOp = BoolOp {isEq :: Bool} deriving (Eq, Show)

--maybe T_Eq should represent both T_Eq and T_notEq???
data Type = TypeInt
          | TypeStr
          | TypeBool
            deriving (Eq, Show)


--more tokens to be added
data Token = T_id String
           | T_LBrace
           | T_RBrace
           | T_LParen
           | T_RParen
           | T_intOp
           | T_boolOp BoolOp
           | T_true
           | T_false
           | T_string String
           | T_if
           | T_while
           | T_print
           | T_type Type
           | T_EOP
           | Invalid
                deriving (Eq, Show)


data State = State String String [Token] Int Int Int deriving (Show)



getKey :: (Int, Char) -> Int
getKey (k, p) = k
getPair :: (Int, Char) -> Char
getPair (k, p) = p
--getSymbols :: Int -> [Char]
--getSymbols k = map (getPair) (filter ((==k).getKey) symbolTable)
getNum :: Char -> Int
getNum s = getKey (head (filter ((==s).getPair) symbolTable))

--makes a blank state with input [to be tokenized].
newState :: String -> State
newState i = State i [] [] 0 0 0

--takes a string, returns a list of tokens
tokenize :: String -> [Token]
tokenize "" = []
tokenize i = tokenizeHelp (newState i)

--INPUT: NON-EMPTY STRING
--needs more conditions!!!
tokenizeHelp :: State -> [Token]
--empty input
tokenizeHelp s@(State [] b t n l c) = t
--one char input
tokenizeHelp s@(State (fst:[]) b t n l c) = tokenizeHelp (processState (State [] (b++[fst]) t n l c))
--multiple char input
tokenizeHelp s@(State (fst:i) b t n l c) = tokenizeHelp (processState (State i (b++[fst]) t n l c))



--takes a state, add input to buffer, read buffer
--needs more conditions!!!
processState :: State -> State
processState s@(State i b t n l c) =
    --token can be made
    if((makeToken s) /= Invalid)
        then (State i "" (t++[(makeToken s)]) (makePath (makeToken s) s) l (c+(length$makeTerminal$(makeToken s))))
    --buffer is a space or tab
    else if ((b == " ")||(b == "\t"))
        then (State i "" t n l (c+1))
    --buffer is a new line
    else if (b == "\n")
        then (State i "" t n (l+1) 0)
    --will eventually enter a string loop for another "
    --buffer is a " (starting a string)
    else if (b == "\"")
        then (stringLoop (State (tail i) "" t n l (c+1)))
    --nothing to be processed
    else if ((True) `elem` (map (==(last b)) (map (getPair) symbolTable)))
        then s
    --unexpected token error for unrecognized input
    else error ("\nLEXER: unexpected token: \""++b++"\" at line "++(show l)++" character "++(show c))

stringLoop :: State -> State
--ending " not found
stringLoop s@(State [] b t n l c) = error ("\nLEXER: end of string not found on line "++(show l))
--first of input is "
stringLoop s@(State (fst@('\"'):i) b t n l c) = (State i "" (t++[(makeToken (State i (b++[fst]) t n l c))]) n l (c+1))
--first of input is VALID character
stringLoop s@(State (fst:i) b t n l c) = stringLoop (State i (b++[fst]) t n l (c+1))


--is the first of input "?
    --if (fst == '\"') then (State i "" (t++(makeToken (State i (fst:b) t n l c))) n l (c+1))
    --else
    --    stringLoop (State i (b++[fst]) t n l (c+1))


--returns an int which is the desired path for a
--      given state and token.
--Token is the Token of the TRAVELED PATH
makePath :: Token -> State -> Int
makePath tkn s@(State _ b _ n _ _) = head (filter (==(getNum (last (makeTerminal tkn)))) (getAdjacentCons s))


--Takes a state, reads the buffer, makes a token
makeToken :: State -> Token
makeToken s@(State _ b _ n l c) =
    if      (b=="{")  then      T_LBrace
    else if (b=="}")  then      T_RBrace
    else if (b=="(")  then      T_LParen
    else if (b==")")  then      T_RParen
    else if (b=="+")  then      T_intOp
    else if (b=="!=")  then     T_boolOp (BoolOp False)
    else if (b=="==")  then     T_boolOp (BoolOp True)
    else if (b=="true")  then   T_true
    else if (b=="false")  then  T_false
    else if (b=="while")  then  T_while
    else if (b=="print")  then  T_print
    else if (b=="int") then     T_type TypeInt
    else if (b=="string") then  T_type TypeStr
    else if (b=="boolean") then T_type TypeBool
    else if (((head b) == '\"') && ((last b) == '\"')) then (T_string (tail (init b)))
    else if (b=="$") then       T_EOP
    else Invalid


makeTerminal :: Token -> [Char]
makeTerminal T_LBrace = "{"
makeTerminal T_RBrace = "}"
makeTerminal T_LParen = "("
makeTerminal T_RParen = ")"
makeTerminal T_intOp  = "+"
makeTerminal (T_boolOp (BoolOp True))  = "=="
makeTerminal (T_boolOp (BoolOp False)) = "!="
makeTerminal T_true = "true"
makeTerminal T_false = "false"
makeTerminal T_if =    "if"
makeTerminal T_while = "while"
makeTerminal T_print = "print"
makeTerminal (T_type TypeInt) =  "int"
makeTerminal (T_type TypeStr) =  "string"
makeTerminal (T_type TypeBool) = "boolean"
makeTerminal (T_string s) = s
makeTerminal T_EOP = "$"
makeTerminal Invalid = error "Cannot tokenize buffer!" 

--takes a state, looks at the number, returns list of possible state paths
getAdjacentCons :: State -> [Int]
getAdjacentCons s@(State _ _ _ n _ _) = filter (>0) (stateCons!!n)


