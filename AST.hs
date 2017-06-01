module AST where

import LanguageData
import Grammar
import Data.Tree



treePROGRAM :: PROGRAM -> [Token] -> Tree String
treePROGRAM p@(Program b) ts = Node "Program" [(treeBLOCK b (init ts))]

treeBLOCK :: BLOCK -> [Token] -> Tree String
treeBLOCK b@(Block sl) ts = Node "Block" [(treeSTMTlist sl (tail (init ts)))]

treeSTMTlist :: [STMTlist] -> [Token] -> Tree String
treeSTMTlist sl ts = undefined
                        --Node "Statement List" (treeSTMTloop sl ts [])

--treeSTMTlist (s:[]) ts = treeSTMT 
--treeSTMTlist (s:sl) ts = Node "Statement List" ( (treeSTMT s):(treeSTMTlist sl ts) )


--treeSTMTloop :: [STMTlist] -> [Token] -> Forest String -> Forest String
--treeSTMTloop [] ts fr = fr 
--treeSTMTloop ((s@(STMTlistNode stmt)):sl) ts fr = treeSTMT stmt sl ts fr

--treeSTMT :: STMT -> [STMTlist] -> [Token] -> Forest String -> Forest String
--treeSTMT (stmt@(PrintSTMT e)) sl ts fr = (treeSTMT):








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

--unfoldTree examples
t1 = unfoldTree (\x -> (x,[])) "a"
t2 = unfoldTree (\x -> case x of "a" -> (x, ["b","c"]); _ -> (x,[])) "a"