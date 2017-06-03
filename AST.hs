module AST where

import LanguageData
import Grammar
import Data.Tree
import Data.List

--"delete" function in Data.List


makeChild :: Tree String -> Tree String -> Tree String
makeChild p@(Node n lst) c = Node n (lst++[c])

makeChildren :: Tree String -> Forest String -> Tree String
makeChildren p@(Node n lst) cs = Node n (lst++cs)


data SymbolTable = SymbolTable [IDRow]

data IDRow = IDRow ID TYPE ENV

data ENV = ENV String [ID]

addRow :: SymbolTable -> IDRow -> SymbolTable
addRow s@(SymbolTable idrows) row = (SymbolTable (row:idrows))

makeTable :: String -> SymbolTable
makeTable s = tableHelp s 0 (SymbolTable [])

tableHelp :: String -> Int -> SymbolTable -> SymbolTable
tableHelp input envNum table = undefined

data Input = Ib  BLOCK
           | Isl [STMTlist]
           | Is  STMT
           | Ie  EXPR
           | EMPTY
                deriving (Eq, Show)


data State = State Input [Token] [STMTlist] (Tree String)
                deriving (Eq, Show)


--------------------------------AST CREATION FUNCTION AND HELPERS----------------------------------


makeAST :: PROGRAM -> [Token] -> Tree String
makeAST p@(Program b@(Block ((n@(STMTlistNode stmt)):stmtLst))) ts = 
    astLoop (State (Is stmt) ((drop 1) $ init $ init ts) (stmtLst) (Node "<BLOCK>" []))

--Initial Input in State is a STMT, BLOCK #0 disected in makeAST
astLoop :: State -> Tree String
--BASE CASE: If no tokens are left, return the tree
astLoop state@(State _ [] _ tr) = tr

--STMT
--VarDecl
--last statement
astLoop state@(State (i@(Is (stmt@(VarDeclSTMT t id)))) ts [] tr) = astLoop 
    ( State (EMPTY) (drop 2 ts) [] ( tr `makeChild` (Node "<Variable Declaration>" [(Node (show t) []), (Node (show id) [])]) ) )
--statements to go
astLoop state@(State (i@(Is (stmt@(VarDeclSTMT t id)))) ts ((n@(STMTlistNode s)):sl) tr) = astLoop 
    ( State (Is s) (drop 2 ts) sl ( tr `makeChild` (Node "<Variable Declaration>" [(Node (show t) []), (Node (show id) [])]) ) )


--Assign Statement
--statements to go
--astLoop state@(State (i@(Is (stmt@(VarDeclSTMT t id)))) ts ((n@(STMTlistNode s)):sl) tr) = astLoop 
--    ( State (EMPTY) (drop 2 $)  )


astLoop state@(State i ts sl tr) = Node "ERROR: pattern not reached!" []


--make a condition for BLOCK!!! just because the first one is handled, doesn't mean if and while statements won't require this.









astLoopB :: BLOCK -> [Token] -> Tree String
astLoopB b@(Block ((n@(STMTlistNode stmt)):stmtLst)) ts = 
                Node "Block" [Node "StatementList" (astLoopSL stmt (tail$init ts) stmtLst)]

astLoopSL :: STMT -> [Token] -> [STMTlist] -> Forest String
astLoopSL stmt ts sl = (astLoopS stmt ts sl)

astLoopS :: STMT -> [Token] -> [STMTlist] -> Forest String
astLoopS stmt@(PrintSTMT expr) ts (s:sl) = undefined


astLoopS _ _ _ = error "PrintSTMT pattern not reached!!!"


----------------------------------------------------------------------------------------------------
































































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