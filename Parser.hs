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

testP :: Parser (Tree Token)
testP = spaceP *> digitP



--SHOULD NOT RETURN A T_ID!!!! ONLY TEMPORARY FOR CONSISTENT TYPES
charP :: Parser (Tree Token)
charP = ((string "a") *> pure (Node (T_id "a") []))
     <|>((string "b") *> pure (Node (T_id "b") []))
     <|>((string "c") *> pure (Node (T_id "c") []))
--come back to!

spaceP :: Parser (Tree Token)
spaceP = (string " ") *> pure (Node T_space [])
--

digitP :: Parser (Tree Token)
digitP = ((string "0") *> pure (Node (T_int "0") []))
     <|> ((string "1") *> pure (Node (T_int "1") []))
     <|> ((string "2") *> pure (Node (T_int "2") []))
     <|> ((string "3") *> pure (Node (T_int "3") []))
     <|> ((string "4") *> pure (Node (T_int "4") []))
     <|> ((string "5") *> pure (Node (T_int "5") []))
     <|> ((string "6") *> pure (Node (T_int "6") []))
     <|> ((string "7") *> pure (Node (T_int "7") []))
     <|> ((string "8") *> pure (Node (T_int "8") []))
     <|> ((string "9") *> pure (Node (T_int "9") []))
--     

boolOpP :: Parser (Tree Token)
boolOpP = ((string "==") *> pure (Node (T_boolOp (BoolOp True)) [])) 
      <|> ((string "!=") *> pure (Node (T_boolOp (BoolOp False)) []))
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





