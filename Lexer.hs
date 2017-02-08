module Lexer where

data TOKEN = T_name [Char]
		   | T_LParen
		   | T_RParen
		   | Invalid
		   		deriving (Show)

tokens :: [[Char]]
tokens =
	[
		"(",
		")"
	]

tokenize :: [Char] -> [TOKEN]
tokenize "" = []
tokenize x = tokenizeHelper x "" []

tokenizeHelper :: [Char] -> [Char] -> [TOKEN] -> [TOKEN]
tokenizeHelper "" _ tokens = tokens
tokenizeHelper (fst:inpt) buff tkns = 
	if (elem [fst] tokens) then tokenizeHelper inpt buff tkns++[(makeTOKEN [fst])]
	else error "ERROR"




makeTOKEN :: [Char] -> TOKEN
makeTOKEN "(" = T_LParen
makeTOKEN ")" = T_RParen
makeTOKEN x = Invalid
