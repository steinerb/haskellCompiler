module Lexer where

import LanguageData
import Data.List
import Data.Maybe
import Data.Char


data State = State String String [Token] Int Int Int deriving (Show)

getKey :: (Int, Char) -> Int
getKey (k, p) = k
getPair :: (Int, Char) -> Char
getPair (k, p) = p
getSymbols :: Int -> [Char]
getSymbols k = map (getPair) (filter ((==k).getKey) progressions)
getNum :: Char -> Int
getNum s = getKey (head (filter ((==s).getPair) progressions))

--makes a blank state with input [to be tokenized].
newState :: String -> State
newState i = State i [] [] 0 1 1

lookAhead :: State -> Maybe Char
lookAhead s@(State [] b t n l c) = Nothing
lookAhead s@(State (fst:i) b t n l c) = Just fst

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
processState :: State -> State
processState s@(State i b t n l c) =
    --check to make sure $ is the last character in i!

    --token can be made
    if((makeToken s) /= Invalid)
        then (State i "" (t++[(makeToken s)]) (makePath (makeToken s) s) l (c+(length$makeTerminal$(makeToken s))))
    --will eventually enter a string loop for another "
    --buffer is a " (starting a string)
    else if (b == "\"")
        then (processString s)
        --buffer is = 
    else if (b == "=") then
        --lookahead not =
        if (lookAhead s /= (Just '='))
            then (State i "" (t++[(makeToken s)]) (makePath (makeToken s) s) l (c+(length$makeTerminal$(makeToken s))))
        --lookahead is =
        else
            (State i b t (makePath (makeToken s) s) l c)
    --buffer is a VALID id [a-z] with whitespace in lookahead
    else if ( ((lookAhead s == (Just ' ') || (lookAhead s == (Just '\t')) || (lookAhead s == (Nothing)))) && (b `elem` validCharsS) )
        then (State i "" (t++[(makeToken s)]) (makePath (makeToken s) s) l (c+(length$makeTerminal$(makeToken s))))
    --buffer is a space or tab or new line
    else if ((b == " ")||(b == "\t")||(b == "\n"))
        then (State i "" (t++[T_space]) n (l+1) (c+1))
    --buffer is a new line
    --else if (b == "\n")
    --    then (State i "" t n (l+1) 0)
    --nothing to be processed
    else if ((True) `elem` (map (==(last b)) (map (getPair) progressions)))
        then s
    --unexpected token error for unrecognized input
    else error ("\nLEXER: unexpected token: \""++b++"\" at line "++(show l)++" character "++(show c))

--s is for State, NOT String
processString :: State -> State
--string not closed error condition 1
processString s@(State [] b t n l c) = error ("LEXER: string not closed on line "++(show l))
--string not closed error condition 2 ELSE IFS check for invalid string data ELSE valid input
processString s@(State i b t n l c) =
    if ('\"' `notElem` i)
        then error ("LEXER: string not closed on line "++(show l))
    --Uppercase or Digit error
    else if ((True `elem` (map (isUpper) stringData)) || (True `elem` (map (isDigit) stringData)))
        then error ("LEXER: capital letter or digit detected in string starting on line "++(show l)++" character "++(show c))
    --good to go!!!
    else
        (State (tail (dropWhile (/='\"') i)) "" (t++[T_string (stringData)]) n l (c+(length stringData)))
    where stringData = ((takeWhile (/='\"') i))

--returns an int which is the desired path for a
--      given state and token.
--Token is the Token of the TRAVELED PATH
makePath :: Token -> State -> Int
makePath tkn s@(State _ b _ n _ _) = head (filter (==(getNum (last (makeTerminal tkn)))) (getAdjacentCons s))


--Takes a state, reads the buffer, makes a token
makeToken :: State -> Token
makeToken s@(State i b t n l c) =
    if      (b=="{")  then                                  T_LBrace
    else if (b=="}")  then                                  T_RBrace
    else if (b=="(")  then                                  T_LParen
    else if (b==")")  then                                  T_RParen
    else if (b=="+")  then                                  T_intOp
    else if ((b=="=") && (lookAhead s) /= Just '=')  then   T_assign
    else if (b=="$") then                                   T_EOP
    else if (b==" ") then                                   T_space
    else if (b=="!=")  then                                 T_boolOp (BoolOp False)
    else if (b=="==")  then                                 T_boolOp (BoolOp True)
    else if (b=="true")  then                               T_true
    else if (b=="false")  then                              T_false
    else if (b=="if")  then                                 T_if
    else if (b=="while")  then                              T_while
    else if (b=="print")  then                              T_print
    else if (b=="int") then                                 T_type TypeInt
    else if (b=="string") then                              T_type TypeStr
    else if (b=="boolean") then                             T_type TypeBool
    --T_int
    else if (b `elem` validDigitsS) then (T_int b)
    --T_string
    else if (((head b) == '\"') && ((last b) == '\"') && (length b > 1)) then (T_string b)
    --T_id OLD
    --else if ( ((lookAhead s == (Just ' ')) || (lookAhead s == (Just '\t')) || (lookAhead s == Nothing)) && ((b == "a" ) || (b == "b" ) || (b == "c" ) || (b == "d" ) || (b == "e" ) || (b == "f" ) || (b == "g" ) || (b == "h" ) || (b == "i" ) || (b == "j" ) || (b == "k" ) || (b == "l" ) || (b == "m" ) || (b == "n" ) || (b == "o" ) || (b == "p" ) || (b == "q" ) || (b == "r" ) || (b == "s" ) || (b == "t" ) || (b == "u" ) || (b == "v" ) || (b == "w" ) || (b == "x" ) || (b == "y" ) || (b == "z" )) )
    --        then T_id b
    --T_id
    else if ( ( (lookAhead s == (Just ' ')) || (lookAhead s == (Just '\t')) || (lookAhead s == Nothing) || ( (fromJust (lookAhead s)) `notElem` validCharsC) ) && (b `elem` validCharsS) )
            then T_id b
    else Invalid


makeTerminal :: Token -> [Char]
makeTerminal T_LBrace = "{"
makeTerminal T_RBrace = "}"
makeTerminal T_LParen = "("
makeTerminal T_RParen = ")"
makeTerminal T_intOp  = "+"
makeTerminal T_assign  = "="
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
makeTerminal (T_string str) = str
makeTerminal (T_int str) = str
makeTerminal (T_id s) = s
makeTerminal T_EOP = "$"
makeTerminal T_space = "space"
makeTerminal Invalid = error "Cannot tokenize buffer!" 

--takes a state, looks at the number, returns list of possible state paths
getAdjacentCons :: State -> [Int]
getAdjacentCons s@(State _ _ _ n _ _) = filter (>0) (stateCons!!n)


