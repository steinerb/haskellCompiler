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

