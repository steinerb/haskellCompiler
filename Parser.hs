module Parser where

import LanguageData
import Grammar
import Data.Tree


import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.Parsec (parse)

import Control.Applicative
import Control.Monad




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

