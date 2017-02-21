module Lexer where

import Data.List


data BoolOp = BoolOp {isEq :: Bool} deriving (Eq, Show)

--maybe T_Eq should represent both T_Eq and T_notEq???
data Type = TypeInt
          | TypeStr
          | TypeBool
            deriving (Eq, Show)


--more tokens to be added
data Token = T_name String
           | T_LBrace
           | T_RBrace
           | T_LParen
           | T_RParen
           | T_intOp
           | T_boolOp BoolOp
           | T_if
           | T_while
           | T_print
           | T_type Type
           | T_EOP
           | Invalid
                deriving (Eq, Show)


data State = State String String [Token] Int deriving (Show)

--state 8: (!|=) -> =
--TO COMPLETE FOR NOW: if, while, print, int,
stateCons :: [[Int]]
stateCons =
    --    {   }   (   )   +   !   =   i   f   w   h   l   e   p   r   n   t   s   g   b   o   a
    [
        [ 1,  2,  3,  4,  5,  6,  7,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 23, -1, 28, -1, -1 ], -- 0: 
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], -- 1: { found
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], -- 2: } found
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], -- 3: ( found
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], -- 4: ) found
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], -- 5: + found
        [-1, -1, -1, -1, -1,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], -- 6: ! found NOT EQ
        [-1, -1, -1, -1, -1, -1,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], -- 7: = found EQ
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], -- 8: = found END OF NOT EQ & EQ
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], -- 9: w found WHILE
        [-1, -1, -1, -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --10: h found WHILE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --11: i found WHILE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --12: l found WHILE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --13: e found WHILE
        [-1, -1, -1, -1, -1, -1, -1, -1, 15, -1, -1, -1, -1, -1, -1, 16, -1, -1, -1, -1, -1, -1 ], --14: i found IF, INT
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --15: f found IF
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 17, -1, -1, -1, -1, -1 ], --16: n found INT
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 19, -1, -1, -1, -1, -1, -1, -1, -1 ], --18: p found PRINT
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 20, -1, -1, -1, -1, -1, -1, -1 ], --19: r found PRINT
        [-1, -1, -1, -1, -1, -1, -1, 21, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --20: i found PRINT
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 22, -1, -1, -1, -1, -1 ], --21: n found PRINT     
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --22: t found PRINT  
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, -1 ], --23: s found STRING
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, -1, -1, -1, -1, -1 ], --24: t found STRING
        [-1, -1, -1, -1, -1, -1, -1, 26, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --25: r found STRING
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, -1 ], --26: i found STRING *
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 28, -1, -1, -1 ], --27: n found STRING *
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --28: g found STRING
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 30, -1 ], --29: b found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1 ], --30: o found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --31: o found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, -1, -1, -1, -1, -1, -1 ], --32: l found BOOLEAN *
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 34 ], --33: e found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 35, -1, -1, -1, -1, -1, -1 ], --34: a found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ]  --35: n found BOOLEAN
    ]
        --[-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ] --18: 


--CONDITION: cannot have multiple characters per number
symbolTable :: [(Int, Char)]
symbolTable =
    [
        (1,  '{'),
        (2,  '}'),
        (3,  '('),
        (4,  ')'),
        (5,  '+'),
        (6,  '!'),
        (7,  '='),
        (8,  '='),
        (9,  'w'),
        (10, 'h'),
        (11, 'i'),
        (12, 'l'),
        (13, 'e'),
        (14, 'i'),
        (15, 'f'),
        (16, 'n'),
        (17, 't'),
        (18, 'p'),
        (19, 'r'),
        (20, 'i'),
        (21, 'n'),
        (22, 't'),
        (23, 's'),
        (24, 't'),
        (25, 'r'),
        (26, 'i'),
        (27, 'n'),
        (28, 'g'),
        (29, 'b'),
        (30, 'o'),
        (31, 'o'),
        (32, 'l'),
        (33, 'e'),
        (34, 'a'),
        (35, 'n')
    ]
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
newState i = State i [] [] 0

--takes a string, returns a list of tokens
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
    --unexpected token error for unrecognized input
    else error ("\nLEXER:unexpected token: "++b)


--returns an int which is the desired path for a
--      given state and token.
--Token is the Token of the TRAVELED PATH
makePath :: Token -> State -> Int
makePath tkn s@(State _ b _ n) = head (filter (==(getNum (last (makeTerminal tkn)))) (getAdjacentCons s))

--ALTERNATIVE VALID SYNTAX:
--makeToken :: State -> Token
--makeToken s@(State _ b@(")") _ n))  = T_LParen
--makeToken s@(State _ b@("(") _ n))  = T_RParen
--makeToken s@(State _ "if" _ n)) = T_if
--makeToken s@(State _ "in" _ n)) = T_in
--makeToken x = Invalid

--Takes a state, reads the buffer, makes a token
makeToken :: State -> Token
makeToken s@(State _ b _ n) =
    if      (b=="{")  then T_LBrace
    else if (b=="}")  then T_RBrace
    else if (b=="(")  then T_LParen
    else if (b==")")  then T_RParen
    else if (b=="+")  then T_intOp
    else if (b=="!=")  then T_boolOp (BoolOp False)
    else if (b=="==")  then T_boolOp (BoolOp True)
    else if (b=="if")  then T_if
    else if (b=="while")  then T_while
    else if (b=="print")  then T_print
    else if (b=="int") then T_type TypeInt
    else if (b=="string") then T_type TypeStr
    else if (b=="boolean") then T_type TypeBool
    else if (b=="$") then T_EOP
    else Invalid


makeTerminal :: Token -> [Char]
makeTerminal T_LBrace = "{"
makeTerminal T_RBrace = "}"
makeTerminal T_LParen = "("
makeTerminal T_RParen = ")"
makeTerminal T_intOp  = "+"
makeTerminal (T_boolOp (BoolOp True))  = "=="
makeTerminal (T_boolOp (BoolOp False)) = "!="
makeTerminal T_if =    "if"
makeTerminal T_while = "while"
makeTerminal T_print = "print"
makeTerminal (T_type TypeInt) =  "int"
makeTerminal (T_type TypeStr) =  "string"
makeTerminal (T_type TypeBool) = "boolean"
makeTerminal T_EOP = "$"
makeTerminal Invalid = error "Cannot tokenize buffer!" 

--takes a state, looks at the number, returns list of possible state paths
getAdjacentCons :: State -> [Int]
getAdjacentCons s@(State _ _ _ n) = filter (>0) (stateCons!!n)


