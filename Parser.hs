module Parser where

import LanguageData
import Grammar
import Data.Tree


import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.Parsec (parse)

import Control.Applicative
import Control.Monad



parseTEST = (parse boolValP "test" "true") 


--NEED TO CHANGE (Tree Token) AS Parser PARAMETER WITH TYPES FROM THE GRAMMAR!!






-------PARSER START-------

testP :: Parser (Tree String)
testP = spaceP *> digitP



--SHOULD NOT RETURN A T_ID!!!! ONLY TEMPORARY FOR CONSISTENT TYPES
charP :: Parser (Tree String)
charP = ((string "a") *> pure (Node (show A) []))
     <|>((string "b") *> pure (Node (show B) []))
     <|>((string "c") *> pure (Node (show C) []))
--come back to!

spaceP :: Parser (Tree String)
spaceP = (string " ") *> pure (Node (show SPACE) [])
--

digitP :: Parser (Tree String)
digitP = ((string "0") *> pure (Node (show ZERO) []))
     <|> ((string "1") *> pure (Node (show ONE) []))
     <|> ((string "2") *> pure (Node (show TWO) []))
     <|> ((string "3") *> pure (Node (show THREE) []))
     <|> ((string "4") *> pure (Node (show FOUR) []))
     <|> ((string "5") *> pure (Node (show FIVE) []))
     <|> ((string "6") *> pure (Node (show SIX) []))
     <|> ((string "7") *> pure (Node (show SEVEN) []))
     <|> ((string "8") *> pure (Node (show EIGHT) []))
     <|> ((string "9") *> pure (Node (show NINE) []))
--     

boolOpP :: Parser (Tree String)
boolOpP = ((string "==") *> pure (Node (show EQUALS) [])) 
      <|> ((string "!=") *> pure (Node (show NOTEQUALS) []))
--

boolFalseP :: Parser (Tree String)
boolFalseP = (string (show FALSE)) *> pure (Node (show FALSE) [])
boolTrueP :: Parser (Tree String)
boolTrueP = (string (show TRUE)) *> pure (Node (show TRUE) [])

boolValP :: Parser (Tree String)
boolValP = boolTrueP <|> boolFalseP
--


intOpP :: Parser (Tree String)
intOpP = (string (show INTOP)) *> pure (Node (show INTOP) [])
--



-------PARSER STOP-------




--TREE METHODS
stringifyTokenTree :: (Tree Token) -> (Tree String)
stringifyTokenTree t = fmap show t 


makeChild :: Tree String -> Tree String -> Tree String
makeChild p@(Node n lst) c = Node n (c:lst)



--TREE EXAMPLES!!!

programTree :: Tree String
programTree = Node "Program" []

tokenTree :: Tree Token
tokenTree = Node T_LBrace []



myTree :: Tree String
myTree = Node "rootNode" 
         [
            Node "BranchA-0" 
            [
                Node "BranchB-0" [],
                Node "BranchB-1" []
            ],
            Node "BranchA-1" 
            [
                Node "BranchC-0" [],
                Node "BranchC-1" []
            ]
         ]





