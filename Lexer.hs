module Lexer where

data Token = T_name String
		   | T_LParen
		   | T_RParen
		   | T_if
		   | T_in
		   | Invalid
		   		deriving (Show)

makeToken :: String -> Token
makeToken "(" = T_LParen
makeToken ")" = T_RParen
makeToken "if" = T_if
makeToken "in" = T_in
makeToken x = error "ERROR!!!"

symbolTable :: [(Int, String)]
symbolTable =
	[
		(1, "("),
		(2, ")"),
		(3, "i"),
		(4, "n"),
		(5, "f")
	]
getKey :: (Int, String) -> Int
getKey (k, p) = k
getPair :: (Int, String) -> String
getPair (k, p) = p
getSymbol :: Int -> String
getSymbol k = getPair (head (filter ((==k).getKey) symbolTable))

data State = State String String [Token] Int deriving (Show)

-- -9 => unexpected token error
-- -8 => token found
stateCons :: [[Int]]
stateCons =
	[
		[ 1,  2,  3, -9, -9],
		[-8, -8, -8, -8, -8], --( found
		[-8, -8, -8, -8, -8], --) found
		[-9, -9, -9,  5,  4], --i found
		[-8, -8, -8, -8, -8], --in found
		[-8, -8, -8, -8, -8]  --if found
	]

newState :: String -> State
newState i = State i [] [] 0

--tokenize :: String -> [Token]
--tokenize "" = []
--tokenize i = tokenizeHelp (newState i)


tokenizeHelp :: State -> [Token]
--shouldn't be (n+1), should direct you to proper state
tokenizeHelp s@(State (f:i) b t n) = tokenizeHelp (State i (b++[f]) t (n+1))
tokenizeHelp s@(State _ _ _ 1) = tokenizeHelp (acceptingState s)
tokenizeHelp s@(State [] b t _) = t

acceptingState :: State -> State
acceptingState s@(State i b t n) = (State i [] (t++[(makeToken b)]) n)

