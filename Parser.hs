module Parser where

import LanguageData
import Grammar
import Data.Tree


import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.Parsec (parse)

import Control.Applicative
import Control.Monad


--drawTEST = drawTree (parseTEST)
parseTEST = (parse digitP "test" "0") *> (parse digitP "test2" "1")

-------PARSER START-------

intOpP :: Parser (Tree Token)
intOpP = (string (show T_intOp)) *> pure (Node T_intOp [])
--

boolFalseP :: Parser (Tree Token)
boolFalseP = (string (show T_false)) *> pure (Node T_false [])
boolTrueP :: Parser (Tree Token)
boolTrueP = (string (show T_true)) *> pure (Node T_true [])

boolValP :: Parser (Tree Token)
boolValP = boolTrueP <|> boolFalseP
--

boolOpP :: Parser (Tree Token)
boolOpP = ((string "==") *> pure (Node (T_boolOp (BoolOp True)) [])) 
      <|> ((string "!=") *> pure (Node (T_boolOp (BoolOp False)) []))
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


-------PARSER STOP-------

--TREES


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

