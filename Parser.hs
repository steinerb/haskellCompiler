module Parser where

import LanguageData
import Grammar
import Data.Tree


--the parser will validate token list. return list if good to go, error otherwise.
--parseTokens :: [Token] -> [Token]

parseTest :: String
parseTest = "\nPARSER: parser reached!!"




parse :: [Token] -> [Token]
parse ts = ts