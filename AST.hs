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
--VarDecl1
--last statement
astLoop state@(State (i@(Is (stmt@(VarDeclSTMT t id)))) ts [] tr) = astLoop 
    ( State (EMPTY) (drop 2 ts) [] ( tr `makeChild` (Node "<Variable Declaration>" [(Node (show t) []), (Node (show id) [])]) ) )
--statements to go
astLoop state@(State (i@(Is (stmt@(VarDeclSTMT t id)))) ts ((n@(STMTlistNode s)):sl) tr) = astLoop 
    ( State (Is s) (drop 2 ts) sl ( tr `makeChild` (Node "<Variable Declaration>" [(Node (show t) []), (Node (show id) [])]) ) )
--AssignStatement
--last statement
astLoop state@(State (i@(Is (stmt@(AssignSTMT id expr)))) ts [] tr) = 
    --if id equal to another id
    if ( ((validIdToken (ts!!2)) == True ) ) 
        then astLoop ( State (EMPTY) (drop 3 ts) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (drop 1 $ init $ show (ts!!2)) [])])) )
    --if id equal to singular BoolVal   [SAME]
    else if (((ts!!2) == T_true) || ((ts!!2) == T_false))
        then astLoop ( State (EMPTY) (drop 3 ts) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (ts!!2)) [])])) )
    --if id equal to string literal     [SAME]
    else if (validStrLitToken (ts!!2) == True)
        then astLoop ( State (EMPTY) (drop 3 ts) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (ts!!2)) [])])) )
    --if id equal to int literal M (must go BEFORE int literal S)
    else if ((validIntSToken (ts!!2)) && (length ts >= 4) && ((ts!!3) == T_intOp))
        then astLoop ( State (EMPTY) (dropWhile (validIntM) (drop 2 ts)) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeWhile (validIntM) (drop 2 ts))) [])])) )
    --if id equal to int literal S      [SAME]
    else if (validIntSToken (ts!!2))
        then astLoop ( State (EMPTY) (drop 3 ts) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (drop 1 $ init $ show (ts!!2)) [])])) )


    --else if ( ((show (ts!!2)) == "true") || ((show (ts!!2)) == "false") )
    --    then astLoop ( State (EMPTY) (drop 3 ts) [] 
    --                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (ts!!2)) [])])) )
    --make more conditions here!!!
    else
             astLoop ( State (EMPTY) (drop 3 ts) [] tr)

    --        else error "only compiling!"
    --astLoop ( State (EMPTY) (drop 2 $)  )


astLoop state@(State i ts sl tr) = Node "ERROR: pattern not reached!" []


--make a condition for BLOCK!!! just because the first one is handled, doesn't mean if and while statements won't require this.






validIdToken :: Token -> Bool
validIdToken id@(T_id c) = if ( (c == "a") || (c == "b") || (c == "c") || (c == "d") || (c == "e") || (c == "f") || (c == "g") || (c == "h") || (c == "i") || (c == "j") || (c == "k") || (c == "l") || (c == "m") || (c == "n") || (c == "o") || (c == "p") || (c == "q") || (c == "r") || (c == "s") || (c == "t") || (c == "u") || (c == "v") || (c == "w") || (c == "x") || (c == "y") || (c == "z") )
                             then True 
                            else False
validIdToken id = False

validStrLitToken :: Token -> Bool
validStrLitToken t@(T_string _) = True
validStrLitToken t = False

validIntSToken :: Token -> Bool
validIntSToken t@(T_int c) = True
validIntSToken t = False

validIntM :: Token -> Bool
validIntM t@(T_int c) = True
validIntM t@(T_intOp) = True
validIntM t = False

----------------------------------------------------------------------------------------------------



