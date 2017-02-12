module Lexer where



data Token = T_name String
           | T_LBrace
           | T_RBrace
           | T_type
           | T_EOP
           | Invalid
                deriving (Eq, Show)


data State = State String String [Token] Int deriving (Show)


--CONDITION: cannot have multiple characters per number
symbolTable :: [(Int, String)]
symbolTable =
    [
        (1, "{"),
        (2, "}"),
        (3, "i"),
        (4, "n"),
        (5, "t"),
        (6, "$")
    ]
getKey :: (Int, String) -> Int
getKey (k, p) = k
getPair :: (Int, String) -> String
getPair (k, p) = p

getSymbols :: Int -> [String]
getSymbols k = map (getPair) (filter ((==k).getKey) symbolTable)

getNum :: String -> Int
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
    else s

--IF LENGTH OF BUFFER IS ONE
--else if ((len b) == 1) then lookAhead b s
--lookAhead :: State -> State
--lookAhead bOld s@(State (fst:i) b _ _) = 
--    if (filter (isInfixOf (makeToken (bOld++[fst]))) (makeToken))


--(stateCons (State connections) logic to be implemented)
--Token -> String -> Last Element -> Int 

--Token is the Token of the TRAVELED PATH
makePath :: Token -> State -> Int
makePath tkn s@(State _ b _ n) = head (filter (==(getNum (last ((makeTerminal tkn)):[]))) (getAdjacentCons s))
--makeToken :: State -> Token
--makeToken s@(State _ b@(")") _ n))  = T_LParen
--makeToken s@(State _ b@("(") _ n))  = T_RParen
--makeToken s@(State _ "if" _ n)) = T_if
--makeToken s@(State _ "in" _ n)) = T_in
--makeToken x = Invalid

makeToken :: State -> Token
makeToken s@(State _ b _ n) =
    if      (b=="{")  then T_LBrace
    else if (b=="}")  then T_RBrace
    else if (b=="int") then T_type
    else if (b=="$") then T_EOP
    else Invalid

--makeToken "k" = T_kill
makeTerminal :: Token -> String
makeTerminal T_LBrace = "{"
makeTerminal T_RBrace = "}"
makeTerminal T_type = "int"
makeTerminal T_EOP = "$"
makeTerminal Invalid = error "Cannot tokenize buffer!" 

--will be used soon!!
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