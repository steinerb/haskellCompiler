module AST where

import LanguageData
import Grammar
import Data.Tree
import Data.List

--"delete" function in Data.List

--Tree Helper Functions
getVal :: Tree String -> String
getVal n@(Node val children) = val

getChildren :: Tree String -> [Tree String]
getChildren n@(Node val children) = children

makeChild :: Tree String -> Tree String -> Tree String
makeChild p@(Node n lst) c = Node n (lst++[c])

makeChildren :: Tree String -> Forest String -> Tree String
makeChildren p@(Node n lst) cs = Node n (lst++cs)

removeQuotes :: Tree String -> Tree String
removeQuotes tr@(Node val children) = (Node (filter (/='\"') val) (fmap removeQuotes children))


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
--      state@(State i ts sl tr)

--BASE CASE: If no tokens are left, return the tree
astLoop state@(State _ [] _  tr) = tr

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
    --if id equal to Boolean Expr Multiple
    else if ((ts!!2) == T_LParen)
        then astLoop ( State (EMPTY) (dropUntilP (drop 2 ts)) []
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeUntilP (drop 2 ts))) [])])) )
    --if id equal to int literal Multiple (must go BEFORE int literal Single)
    else if ((validIntSToken (ts!!2)) && (length ts >= 4) && ((ts!!3) == T_intOp))
        then astLoop ( State (EMPTY) (dropWhile (validIntM) (drop 2 ts)) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeWhile (validIntM) (drop 2 ts))) [])])) )
    --if id equal to BoolValSingle, string literal, or int literal Single
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
    --if id equal to Boolean Expr Multiple
    else if ((ts!!2) == T_LParen)
        then astLoop ( State (Is s) (dropUntilP (drop 2 ts)) sl
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeUntilP (drop 2 ts))) [])])) )
    --if id equal to int literal Multiple (must go BEFORE int literal Single)
    else if ((validIntSToken (ts!!2)) && (length ts >= 4) && ((ts!!3) == T_intOp))
        then astLoop ( State (Is s) (dropWhile (validIntM) (drop 2 ts)) sl 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeWhile (validIntM) (drop 2 ts))) [])])) )
    --if id equal to BoolValSingle, string literal, or int literal Single 
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


--attempt 1:
--[(Node "<While Condition>" [(Node (show $ takeWhile ((("="`isInfixOf`).show)) $ drop 1 $ takeUntilP (drop 1 ts)) []), 
--                                                    (Node (show $ head $ drop 1 $ takeUntilP (drop 1 ts)) []), 
--                                                    (Node (show $ head $ drop 1 $ takeUntilP (drop 1 ts)) [])]),
--attempt 3:
--[(Node "<While Condition>" [(Node (show (takeWhile ((/=(show op)).show) (drop 2 ts))) []), 
--                                        (Node (show op) []), 
--                                        ( Node ( show$drop 1$init (takeUntilP (T_LParen:(drop 1 (dropWhile ((/=(show op)).show) (drop 2 ts))))) ) [] )]),
-----------------------------------------------
--WhileStatement [ONLY WITH MULTIPLE BOOLEXPR!]
--last statement w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ dropUntilP (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(Node "<While Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--statements to go w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ dropUntilP (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(Node "<While Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )

--------------------------------------------
--IfStatement [ONLY WITH MULTIPLE BOOLEXPR!]
--last statement w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ dropUntilP (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(Node "<If Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--statements to go w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ dropUntilP (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(Node "<If Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )


--Statement can't be recognized
astLoop state@(State i ts sl tr) = Node "ERROR: pattern not reached!" []




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

--Table Types
data SymbolTable = SymbolTable [IDRow]          deriving (Eq)

data IDRow = IDRow {pullName :: NAME, pullDType :: DTYPE, pullScope :: SCOPE}              deriving (Eq)

instance Show SymbolTable where
    show table@(SymbolTable rows) = 
        (
            " ------------------------------------------------\n"++
            " |Name\t\t|Type\t\t|Scope\t\t|\n"++
            " |--------------+---------------+---------------|\n"++
            (show rows)
        )

instance Show IDRow where
    show row@(IDRow n t@("boolean") s) = ("|"++n++"\t\t|"++t++"\t|"++(show s)++"\t\t|\n")
    show row@(IDRow n t s) = ("|"++n++"\t\t|"++t++"\t\t|"++(show s)++"\t\t|\n")
    

type NAME = String
type DTYPE = String
type SCOPE = Int

--Table Helper Functions
addRow :: SymbolTable -> IDRow -> SymbolTable
addRow s@(SymbolTable idrows) row = (SymbolTable (idrows++[row]))

isElem :: NAME -> SymbolTable -> Bool
isElem n st@(SymbolTable rows) = n `elem` (map pullName rows)

getRow :: NAME -> SymbolTable -> IDRow
getRow n st@(SymbolTable rows) = head (filter ((==n).pullName) rows)

getType :: NAME -> SymbolTable -> DTYPE
getType n st = pullDType (getRow n st)

getScope :: NAME -> SymbolTable -> SCOPE
getScope n st = pullScope (getRow n st)

--Type Checking Functions
compareType :: String -> String -> SymbolTable -> Bool
compareType a b st = 
    if ((decideType a st) == (decideType b st)) then True
    else False

decideType :: String -> SymbolTable -> String
decideType a st = 
    if ( a `isElem` st ) then (getType a st)
    else if ((decideInt a st) == True) then "int"
    else if ((decideString a st) == True) then "string"
    else "boolean"
--CONDITION: MUST RUN BEFORE decideString TO PICK UP NUMBERS BEFORE LENGTH IS COUNTED!!!
decideInt :: String -> SymbolTable -> Bool
decideInt a st = 
    if ((True `notElem` (map (=='(') a)) && (("0" `isInfixOf` a) || ("1" `isInfixOf` a) || ("2" `isInfixOf` a) || ("3" `isInfixOf` a) || ("4" `isInfixOf` a) || ("5" `isInfixOf` a) || ("6" `isInfixOf` a) || ("7" `isInfixOf` a) || ("8" `isInfixOf` a) || ("9" `isInfixOf` a)) )
        then True 
    else False

decideString :: String -> SymbolTable -> Bool
decideString a st = 
    if ( ((length a) > 1) && (a /= "true") && (a /= "false") && (True `notElem` (map (=='(') a)) ) 
        then True
    else False



--Table creation function and helpers
makeTable :: Tree String -> SymbolTable
makeTable tr@(Node val@("<BLOCK>") children) = processKids children 0 0 (SymbolTable [])
makeTable tr = error "ERROR: Invalid tree as input in makeTable!!!"

processKids :: [Tree String] -> Int -> Int -> SymbolTable -> SymbolTable
--BASE CASE
processKids [] _ _ table = table
--VarDecl
processKids (kid@(Node "<Variable Declaration>" subKids):kids) scope hiScope table =
    processKids kids scope hiScope (table `addRow` (IDRow (getVal (subKids!!1)) (getVal (subKids!!0)) scope))
--AssignStatement
processKids (kid@(Node "<Assign Statement>" subKids):kids) scope hiScope table = 
    --MAKE A SCOPE CHECK CONDITION HERE!!!
    --TYPECHECK ERROR
    if ( (compareType (getVal (subKids!!0)) (getVal (subKids!!1)) table) == False )
        then error ("ERROR: TYPECHECK FAILED: "++(getVal (subKids!!0))++" and "++(getVal (subKids!!1))++" are of different types!")
    else
        processKids kids scope hiScope table



--PATTERN NOT REACHED
processKids _ _ _ _ = error "ERROR: Pattern not reached in processKids!!!"


--creates a sample table
testTable :: SymbolTable
testTable = SymbolTable [(IDRow "a" "int" 0), (IDRow "b" "boolean" 0), (IDRow "c" "string" 0)]



















