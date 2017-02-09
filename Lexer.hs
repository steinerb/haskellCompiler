module Lexer where



data Token = T_name String
           | T_LParen
           | T_RParen
           | T_if
           | T_in
           | Invalid
                deriving (Eq, Show)


data State = State String String [Token] Int deriving (Show)


--CONDITION: cannot have multiple characters per number
symbolTable :: [(Int, String)]
symbolTable =
    [
        (1, "("),
        (2, ")"),
        (3, "i"),
        (5, "n"),
        (4, "f")
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
    else if ((b == " ")||(b == "\t"))
        then (State i "" t n)
    --nothing to be processed
    else s
--kill condition?

--(stateCons (State connections) logic to be implemented)
--Token -> String -> Last Element -> Int 

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
    if      (b=="(")  then T_LParen 
    else if (b==")")  then T_RParen
    else if (b=="if") then T_if
    else if (b=="in") then T_in
    else Invalid

--makeToken "k" = T_kill

makeTerminal :: Token -> String
makeTerminal T_LParen = "("
makeTerminal T_RParen = ")"
makeTerminal T_if = "if"
makeTerminal T_in = "in"
makeTerminal Invalid = error "Cannot tokenize buffer!" 

--will be used soon!!
--takes a state, looks at the number, returns list of possible state paths
getAdjacentCons :: State -> [Int]
getAdjacentCons s@(State _ _ _ n) = filter (>0) (stateCons!!n)




stateCons :: [[Int]]
stateCons =
    --    (   )   i   n   f
    [
        [ 1,  2,  3, -9, -9],
        [-9, -9, -9, -9, -9], --1: ( found
        [-9, -9, -9, -9, -9], --2: ) found
        [-9, -9, -9,  5,  4], --3: proceed to 4 or 5
        [-9, -9, -9, -9, -9], --4: in found
        [-9, -9, -9, -9, -9]  --5: if found
    ]