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
--AssignStatement
--last statement
astLoop state@(State (i@(Is (stmt@(AssignSTMT id expr)))) ts [] tr) = 
    --if id equal to another id
    if ( ((validIdToken (ts!!2)) == True ) ) 
        then astLoop ( State (EMPTY) (drop 3 ts) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (drop 1 $ init $ show (ts!!2)) [])])) )
    --if id equal to Boolean Expr M
    else if ((ts!!2) == T_LParen)
        then astLoop ( State (EMPTY) (dropUntilP (drop 2 ts)) []
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeUntilP (drop 2 ts))) [])])) )
    --if id equal to int literal M (must go BEFORE int literal S)
    else if ((validIntSToken (ts!!2)) && (length ts >= 4) && ((ts!!3) == T_intOp))
        then astLoop ( State (EMPTY) (dropWhile (validIntM) (drop 2 ts)) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeWhile (validIntM) (drop 2 ts))) [])])) )
    --if id equal to BoolValS, string literal, or int literal S 
    else if ( ((ts!!2) == T_true) || ((ts!!2) == T_false) || (validStrLitToken (ts!!2) == True) || (validIntSToken (ts!!2)))
        then astLoop ( State (EMPTY) (drop 3 ts) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (ts!!2)) [])])) )
    --error
    else error "AssignSTMT falsely identified in astLoop!!!"
--statements to go
astLoop state@(State (i@(Is (stmt@(AssignSTMT id expr)))) ts ((n@(STMTlistNode s)):sl) tr) = 
    --if id equal to another id
    if ( ((validIdToken (ts!!2)) == True ) ) 
        then astLoop ( State (Is s) (drop 3 ts) sl 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (drop 1 $ init $ show (ts!!2)) [])])) )
    --if id equal to Boolean Expr M
    else if ((ts!!2) == T_LParen)
        then astLoop ( State (Is s) (dropUntilP (drop 2 ts)) sl
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeUntilP (drop 2 ts))) [])])) )
    --if id equal to int literal M (must go BEFORE int literal S)
    else if ((validIntSToken (ts!!2)) && (length ts >= 4) && ((ts!!3) == T_intOp))
        then astLoop ( State (Is s) (dropWhile (validIntM) (drop 2 ts)) sl 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeWhile (validIntM) (drop 2 ts))) [])])) )
    --if id equal to BoolValS, string literal, or int literal S 
    else if ( ((ts!!2) == T_true) || ((ts!!2) == T_false) || (validStrLitToken (ts!!2) == True) || (validIntSToken (ts!!2)))
        then astLoop ( State (Is s) (drop 3 ts) sl 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (ts!!2)) [])])) )
    --error
    else error "AssignSTMT falsely identified in astLoop!!!"
--PrintStatement
--last statement
astLoop state@(State (i@(Is (stmt@(PrintSTMT expr)))) ts [] tr) = astLoop 
    (State (EMPTY) (dropUntilP (drop 1 ts)) [] 
                        (tr `makeChild` (Node "<Print Statement>" [(Node (show $ drop 1 $ init $ (takeUntilP (drop 1 ts))) [])])) )
--statements to go
astLoop state@(State (i@(Is (stmt@(PrintSTMT expr)))) ts ((n@(STMTlistNode s)):sl) tr) = astLoop 
    (State (Is s) (dropUntilP (drop 1 ts)) sl 
                        (tr `makeChild` (Node "<Print Statement>" [(Node (show $ drop 1 $ init $ (takeUntilP (drop 1 ts))) [])])) )



astLoop state@(State i ts sl tr) = Node "ERROR: pattern not reached!" []


--make a condition for BLOCK!!! just because the first one is handled, doesn't mean if and while statements won't require this.






validIdToken :: Token -> Bool
validIdToken id@(T_id c) = True 
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

takeUntilP :: [Token] -> [Token]
takeUntilP ts = untilP ts 0 []
    where
        untilP ((t@(T_LParen)):ts) i rts = untilP ts (i+1) (rts++[t])
        untilP ((t@(T_RParen)):ts) i rts = untilP ts (i-1) (rts++[t])
        untilP _ 0 rts = rts
        untilP (t:ts) i rts = untilP ts i (rts++[t])

dropUntilP :: [Token] -> [Token]
dropUntilP ts = untilP ts 0
    where
        untilP ((t@(T_LParen)):ts) i = untilP ts (i+1)
        untilP ((t@(T_RParen)):ts) i = untilP ts (i-1)
        untilP ts 0 = ts
        untilP (t:ts) i = untilP ts i


takeUntilB :: [Token] -> [Token]
takeUntilB ts = untilB ts 0 []
    where
        untilB ((t@(T_LBrace)):ts) i rts = untilB ts (i+1) (rts++[t])
        untilB ((t@(T_RBrace)):ts) i rts = untilB ts (i-1) (rts++[t])
        untilB _ 0 rts = rts
        untilB (t:ts) i rts = untilB ts i (rts++[t])

dropUntilB :: [Token] -> [Token]
dropUntilB ts = untilB ts 0
    where
        untilB ((t@(T_LBrace)):ts) i = untilB ts (i+1)
        untilB ((t@(T_RBrace)):ts) i = untilB ts (i-1)
        untilB ts 0 = ts
        untilB (t:ts) i = untilB ts i

----------------------------------------------------------------------------------------------------



