module Parser where

import LanguageData
import Grammar
import Data.Tree


import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.Parsec (parse)

import Control.Applicative
import Control.Monad


--drawTEST = drawTree (parseTEST)
parseTEST = (parse testP "test" " 1") *> (parse testP "test" "1")


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

boolFalseP :: Parser (Tree Token)
boolFalseP = (string (show T_false)) *> pure (Node T_false [])
boolTrueP :: Parser (Tree Token)
boolTrueP = (string (show T_true)) *> pure (Node T_true [])

boolValP :: Parser (Tree Token)
boolValP = boolTrueP <|> boolFalseP
--


intOpP :: Parser (Tree Token)
intOpP = (string (show T_intOp)) *> pure (Node T_intOp [])
--













-------PARSER STOP-------

--TREE EXAMPLES!!!


makeChild :: Tree String -> Tree String -> Tree String
makeChild p@(Node n lst) c = Node n (c:lst)




programTree :: Tree String
programTree = Node "Program" []

tokenTree :: Tree Token
tokenTree = Node T_LBrace []


stringifyTree :: (Tree Token) -> (Tree String)
stringifyTree t = fmap show t 


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

