module AST where

import LanguageData
import Grammar
import Data.Tree
import Data.List
import Data.Text (splitOn, pack, unpack)


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

removeLB :: Tree String -> Tree String
removeLB tr@(Node val children) = (Node (filter (/='[') val) (fmap removeLB children))

removeRB :: Tree String -> Tree String
removeRB tr@(Node val children) = (Node (filter (/=']') val) (fmap removeRB children))


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
--LAST STATEMENT w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr@(BooleanLitM _ op _) b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ dropUntilP (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(Node "<While Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--LAST STATEMENT w/ SINGLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ drop 1 (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(Node "<While Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (drop 1 (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--STATEMENTS TO GO w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr@(BooleanLitM _ op _) b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ dropUntilP (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(Node "<While Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--STATEMENTS TO GO w/ SINGLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ drop 1 (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(Node "<While Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (drop 1 (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )

--------------------------------------------
--IfStatement [ONLY WITH MULTIPLE BOOLEXPR!]
--LAST STATEMENT w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr@(BooleanLitM _ op _) b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ dropUntilP (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(Node "<If Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--LAST STATEMENT w/ SINGLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ drop 1 (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(Node "<If Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (drop 1 (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--STATEMENTS TO GO w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr@(BooleanLitM _ op _) b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ dropUntilP (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(Node "<If Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--STATEMENTS TO GO w/ SINGLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ drop 1 (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(Node "<If Condition>" [Node (show boolExpr) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (drop 1 (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
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
validIntM t@(T_id s) = True 
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

data IDRow = IDRow {pullName :: NAME, pullDType :: DTYPE, pullCurrentScope :: Int}              deriving (Eq)

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

--Scope Type
data SCOPE = SCOPE {current :: Int, scopeKids :: [SCOPE]} deriving (Eq, Show)

--Scope Helper Functions
inScope :: Int -> SCOPE -> Bool
inScope littleNum big@(SCOPE curB kidsB) = if ( littleNum == curB )                             then True
                                      else if ( littleNum `elem` (map current kidsB) )          then True
                                      else if ( True `elem` (map (inScope littleNum) kidsB) )   then True
                                      else    False

--PRECONDITION: toFind `inScope` scopemap == True
fromScope :: Int -> SCOPE -> SCOPE
fromScope toFind scopeMap@(SCOPE cur kids) = if ( toFind == cur) then scopeMap
                                        else if ( (toFind) `elem` (map current kids)  ) then ( head (filter ((==toFind).current) kids) )
                                        else      toFind `fromScope` ( head (filter (inScope toFind) kids) )

addChildScope :: SCOPE -> SCOPE -> SCOPE
addChildScope parent@(SCOPE current children) child = SCOPE current (children++[child])


--hasScope :: SCOPE -> SCOPE -> Bool
--hasScope big@(SCOPE curB [])    _      = False
--hasScope big@(SCOPE curB kidsB) little = if ( (current little) `elem` (map current kidsB) ) then True
--                                    else if ( True `elem` (map (`hasScope` little) kidsB) ) then True
--                                    else    False


--Table Helper Functions
addRow :: SymbolTable -> IDRow -> SymbolTable
addRow s@(SymbolTable idrows) row = (SymbolTable (idrows++[row]))

isElem :: NAME -> SymbolTable -> Bool
isElem n st@(SymbolTable rows) = n `elem` (map pullName rows)

isNotElem :: NAME -> SymbolTable -> Bool
isNotElem n st@(SymbolTable rows) = n `notElem` (map pullName rows)

getRow :: NAME -> SymbolTable -> IDRow
getRow n st@(SymbolTable rows) = head (filter ((==n).pullName) rows)

getType :: NAME -> SymbolTable -> DTYPE
getType n st = pullDType (getRow n st)

getScope :: NAME -> SymbolTable -> Int
getScope n st = pullCurrentScope (getRow n st)



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

--Scope Checking Function (for when needed)
passScopeCheck :: Int -> SCOPE -> SymbolTable -> String -> Bool
passScopeCheck curScope scopeMap table toCheck = 
    if ( (toCheck `isNotElem` table) || ( ((getScope toCheck table) `inScope` (curScope `fromScope` scopeMap)) == False ) )
        then error ("ERROR: SCOPECHECK FAILED: "++toCheck++" is not in scope!!!")
    else True

--Table creation function and helpers
makeTable :: Tree String -> SymbolTable
makeTable tr@(Node val@("<BLOCK>") children) = processKids children 0 0 (SCOPE 0 []) (SymbolTable [])
makeTable tr = error "ERROR: Invalid tree as input in makeTable!!!"

--makeTable main helper function. uses conditionals to scope and type check while generating the table
processKids :: [Tree String] -> Int -> Int -> SCOPE -> SymbolTable -> SymbolTable
--BASE CASE
processKids [] _ _ _ table = table

--VarDecl                   [adds a row to the table]
processKids (kid@(Node "<Variable Declaration>" subKids):kids) curScope hiScope scopeMap table =
    processKids kids curScope hiScope scopeMap (table `addRow` (IDRow (getVal (subKids!!1)) (getVal (subKids!!0)) curScope))

--AssignStatement           [checks:    scope, type]
processKids (kid@(Node "<Assign Statement>" subKids):kids) curScope hiScope scopeMap table = 
    --SCOPECHECK ERROR FOR LHS
    if ( not (passScopeCheck curScope scopeMap table (getVal (subKids!!0))) ) 
        then error "ERROR: SCOPE FAILED: [this error won't ever be reached. this is a passScopeCheck example.]"
    --SCOPECHECK ERROR FOR RHS IF ID
    else if ( (isValidId (getVal (subKids!!1))) && 
                ( 
                    ((getVal (subKids!!1)) `isNotElem` table) || 
                    ( ((getScope (getVal (subKids!!1)) table) `inScope` (curScope `fromScope` scopeMap)) == False ) 
                ) 
            )
        then error ("ERROR: SCOPECHECK FAILED: "++(getVal (subKids!!1))++" is not in scope!!!")
    --SCOPECHECK ERROR FOR RHS IF CONTAINS AN ID (BoolExpr Multiple and possibly others)
    else if ( 
                (containsValidId (getVal (subKids!!1))) && 
                ( False `elem` (map (passScopeCheck curScope scopeMap table) (retrieveValidIds (getVal (subKids!!1)))) ) 
            ) 
        then error "ERROR: SCOPECHECK FAILED: [this error won't ever be reached, passScopeCheck used instead]"
    --TYPECHECK ERROR FOR BOTH
    else if ( (compareType (getVal (subKids!!0)) (getVal (subKids!!1)) table) == False )
        then error  ("ERROR: TYPECHECK FAILED: "++(getVal (subKids!!0))++" and "++(getVal (subKids!!1))++" are of different types!")
    --PASSED SCOPECHECK AND TYPECHECK
    else
        processKids kids curScope hiScope scopeMap table

--PrintStatement
--processKids (kid@(Node "<Print Statement>" subKids):kids) curScope hiScope scopeMap table



--PATTERN NOT REACHED
processKids _ _ _ _ _ = error "ERROR: Pattern not reached in processKids!!!"




isValidId :: String -> Bool
isValidId id = if ( (id == "a") || (id == "b") || (id == "c") || (id == "d") || (id == "e") || (id == "f") || (id == "g") || (id == "h") || (id == "i") || (id == "j") || (id == "k") || (id == "l") || (id == "m") || (id == "n") || (id == "o") || (id == "p") || (id == "q") || (id == "r") || (id == "s") || (id == "t") || (id == "u") || (id == "v") || (id == "w") || (id == "x") || (id == "y") || (id == "z") )
                    then True
               else False



containsValidId :: String -> Bool
containsValidId s = 
    if ( True `elem` (map (isValidId.unpack) (splitOn (pack ",") (pack s))) ) then True
    else False


retrieveValidIds :: String -> [String]
retrieveValidIds s = filter (isValidId) (map (unpack) (splitOn (pack ",") (pack s)))




--for dated code. didn't have the heart to get rid of it.
removeAll :: String -> String -> String
removeAll s toReturn = 
    if (s `isInfixOf` toReturn) 
        then removeAll s (toReturn\\s)
    else 
        toReturn

    





--creates a sample table
testTable :: SymbolTable
testTable = SymbolTable [ 
                            (IDRow "a" "int"        0), 
                            (IDRow "b" "boolean"    0), 
                            (IDRow "c" "string"     1),
                            (IDRow "d" "int"        2) 
                        ]

testScope0 :: SCOPE
testScope0 = (SCOPE 0 [(SCOPE 1 [(SCOPE 2 [])])])

testScope1 :: SCOPE
testScope1 = (SCOPE 1 [(SCOPE 2 [])])

testScope2 :: SCOPE
testScope2 = (SCOPE 2 [])


















