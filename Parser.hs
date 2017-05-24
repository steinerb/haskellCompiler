module Parser where

import LanguageData
import Grammar
import Data.Tree


import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.Parsec (parse)

import Control.Applicative
import Control.Monad







boolFalse :: Parser Bool
boolFalse = (string (show T_false)) *> (pure False)

boolTrue :: Parser Bool
boolTrue = (string (show T_true)) *> (pure True)

bool :: Parser Bool
bool = boolTrue <|> boolFalse











--the parser will validate token list. return list if good to go, error otherwise.
--parseTokens :: [Token] -> [Token]

parseTest :: String
parseTest = "\nPARSER: parser reached!!"


makeChild :: Tree String -> Tree String -> Tree String
makeChild p@(Node n lst) c = Node n (c:lst)

--parse :: [Token] -> Tree String -> Tree String
--parse [] tree = tree
--parse ts tree@(Node _ ns) =
--    if (last ts == T_EOP) then parse (init ts) (Node "Program" )


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

