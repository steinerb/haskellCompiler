module Lexer where

import Data.List


--maybe T_Eq should represent both T_Eq and T_notEq???
data Type = TypeInt
          | TypeStr
          | TypeBool
            deriving (Eq, Show)

--added extra tokens
--more tokens to be added
--
data Token = T_name String
           | T_LBrace
           | T_RBrace
           | T_LParen
           | T_RParen
           | T_intOp
           | T_notEq
           | T_Eq
           | T_if
           | T_while
           | T_print
           | T_type Type
           | T_EOP
           | Invalid
                deriving (Eq, Show)


data State = State String String [Token] Int deriving (Show)



--CONDITION: cannot have multiple characters per number
symbolTable :: [(Int, Char)]
symbolTable =
    [
        (1, '{'),
        (2, '}'),
        (3, 'i'),
        (4, 'n'),
        (5, 't'),
        (6, '$')
    ]
getKey :: (Int, Char) -> Int
getKey (k, p) = k
getPair :: (Int, Char) -> Char
getPair (k, p) = p
--getSymbols :: Int -> [Char]
--getSymbols k = map (getPair) (filter ((==k).getKey) symbolTable)
getNum :: Char -> Int
getNum s = getKey (head (filter ((==s).getPair) symbolTable))


newState :: String -> State
newState i = State i [] [] 0

tokenize :: String -> [Token]
tokenize "" = []
tokenize i = tokenizeHelp (newState i)

--INPUT: NON-EMPTY STRING
--needs more conditions!!!
tokenizeHelp :: State -> [Token]
--empty input
tokenizeHelp s@(State [] b t n) = t
--one char input
tokenizeHelp s@(State (fst:[]) b t n) = tokenizeHelp (processState (State [] (b++[fst]) t n))
--multiple char input
tokenizeHelp s@(State (fst:i) b t n) = tokenizeHelp (processState (State i (b++[fst]) t n))



--takes a state, add input to buffer, read buffer
--needs more conditions!!!
processState :: State -> State
processState s@(State i b t n) =
    --token can be made
    if((makeToken s) /= Invalid)
        then (State i "" (t++[(makeToken s)]) (makePath (makeToken s) s))
    --buffer is a space or tab
    else if ((b == " ")||(b == "\t")||(b == "\n"))
        then (State i "" t n)
    --nothing to be processed
    else if ((True) `elem` (map (==(last b)) (map (getPair) symbolTable)))
        then s
    --unexpected token error for bad input
    else error ("unexpected token: "++b)

--Token is the Token of the TRAVELED PATH
makePath :: Token -> State -> Int
makePath tkn s@(State _ b _ n) = head (filter (==(getNum (last (makeTerminal tkn)))) (getAdjacentCons s))
--makeToken :: State -> Token
--makeToken s@(State _ b@(")") _ n))  = T_LParen
--makeToken s@(State _ b@("(") _ n))  = T_RParen
--makeToken s@(State _ "if" _ n)) = T_if
--makeToken s@(State _ "in" _ n)) = T_in
--makeToken x = Invalid

--types string and boolean need to be added!!
--TO REVERT: change then on 106 to T_type
makeToken :: State -> Token
makeToken s@(State _ b _ n) =
    if      (b=="{")  then T_LBrace
    else if (b=="}")  then T_RBrace
    else if (b=="int") then T_type TypeInt
    else if (b=="string") then T_type TypeStr
    else if (b=="boolean") then T_type TypeBool
    else if (b=="$") then T_EOP
    else Invalid

--types string and boolean need to be added!!
--makeToken "k" = T_kill
makeTerminal :: Token -> String
makeTerminal T_LBrace = "{"
makeTerminal T_RBrace = "}"
makeTerminal (T_type TypeInt) = "int"
makeTerminal (T_type TypeStr) = "string"
makeTerminal (T_type TypeBool) = "boolean"

--makeTerminal T_type = error "Type not known yet!"
makeTerminal T_EOP = "$"
makeTerminal Invalid = error "Cannot tokenize buffer!" 

--takes a state, looks at the number, returns list of possible state paths
getAdjacentCons :: State -> [Int]
getAdjacentCons s@(State _ _ _ n) = filter (>0) (stateCons!!n)


stateCons :: [[Int]]
stateCons =
    --    {   }   i   n   t   $
    [
        [ 1,  2,  3, -9, -9, -9],
        [-9, -9, -9, -9, -9, -9],
        [-9, -9, -9, -9, -9, -9],
        [-9, -9, -9,  4, -9, -9],
        [-9, -9, -9, -9,  5, -9],
        [-9, -9, -9, -9, -9, -9],
        [-9, -9, -9, -9, -9, -9]
    ]