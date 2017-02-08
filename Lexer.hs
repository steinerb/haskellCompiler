module Lexer where

data Token = T_name [Char]
		   | T_LParen
		   | T_RParen
		   | T_if
		   | T_in
		   | Invalid
		   		deriving (Show)

symbolTable :: [(Int, [Char])]
symbolTable =
	[
		(1, "("),
		(2, ")"),
		(3, "i"),
		(4, "n"),
		(5, "f")
	]
getKey :: (Int, [Char]) -> Int
getKey (k, p) = k
getPair :: (Int, [Char]) -> [Char]
getPair (k, p) = p

data State = State [Char] [Char] [Token] Int deriving (Show)

-- -9 => unexpected token error
-- -8 => token found
stateCons :: [[Int]]
stateCons =
	[
		[ 1,  2,  3, -9, -9],
		[-8, -8, -8, -8, -8],
		[-8, -8, -8, -8, -8],
		[-9, -9, -9,  5,  4],
		[-8, -8, -8, -8, -8],
		[-8, -8, -8, -8, -8]
	]

makeState :: [Char] -> State
makeState i = State i [] [] 0

--tokenize :: [Char] -> [Token]
--tokenize "" = []
--tokenize i = tokenizeHelp (makeState i)


tokenizeHelp :: State -> [Token]
tokenizeHelp s@(State [] b t _) = t
tokenizeHelp s@(State (f:i) b t n) = tokenizeHelp (State i (b++[f]) t (n+1))
--tokenizeHelp s@(State _ _ _ 1) tokenizeHelp (acceptingState s)

--acceptingState :: State -> State




