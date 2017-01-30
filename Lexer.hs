module Lexer where

data TOKEN = T_name [Char]
		   | T_LParen
		   | T_RParen
