module AST where

import LanguageData
import Grammar
import Data.Tree
import Data.List

--"delete" function in Data.List





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


data State = State Input [Token] [STMTlist] (Tree String)


--------------------------------AST CREATION FUNCTION AND HELPERS----------------------------------


makeAST :: PROGRAM -> [Token] -> Tree String
makeAST p@(Program b@(Block ((n@(STMTlistNode stmt)):stmtLst))) ts = 
    astLoop (State (Is stmt) ((drop 1) $ init $ init ts) (stmtLst) (Node "BLOCK" []))

--Initial Input in State is a STMT, BLOCK #0 disected in makeAST
astLoop :: State -> Tree String
--if no tokens left, return tree
astLoop state@(State _ [] _ tr) = tr

--make a condition for BLOCK!!! just because the first one is handled, doesn't mean if and while statements won't require this.
--STMT: VarDecl
astLoop state@( State i@(Is stmt@(VarDeclSTMT t id)) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop (State 
                (Is s) 
                (drop 2 ts) 
                sl 
                ( tr `makeChildren` ([ (Node (show t) []), (Node (show id) []) ]) )
            )


astLoop state@(State _ _ _ tr) = tr












astLoopB :: BLOCK -> [Token] -> Tree String
astLoopB b@(Block ((n@(STMTlistNode stmt)):stmtLst)) ts = 
                Node "Block" [Node "StatementList" (astLoopSL stmt (tail$init ts) stmtLst)]

astLoopSL :: STMT -> [Token] -> [STMTlist] -> Forest String
astLoopSL stmt ts sl = (astLoopS stmt ts sl)

astLoopS :: STMT -> [Token] -> [STMTlist] -> Forest String
astLoopS stmt@(PrintSTMT expr) ts (s:sl) = undefined


astLoopS _ _ _ = error "PrintSTMT pattern not reached!!!"


----------------------------------------------------------------------------------------------------


























































--TREE METHODS
stringifyTokenTree :: (Tree Token) -> (Tree String)
stringifyTokenTree t = fmap show t 


makeChild :: Tree String -> Tree String -> Tree String
makeChild p@(Node n lst) c = Node n (c:lst)

makeChildren :: Tree String -> Forest String -> Tree String
makeChildren p@(Node n lst) cs = Node n (cs++lst)



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