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
removeQuotes tr@(Node val children) = 
    --if ( (val == "<Assign Statement>") && () )
    --else
        (Node (filter (/='\"') val) (fmap removeQuotes children))

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



--EXPR
--BooleanExprM
--no statements to go? or doesn't matter?
--Both expr1 and expr2 are BooleanExpr Multiples
astLoop state@(State i@(Ie e@(BooleanEXPR lit@(BooleanLitM expr1@(BooleanEXPR subLit1@(BooleanLitM _ _ _)) op expr2@(BooleanEXPR subLit2@(BooleanLitM _ _ _))))) ts sl tr) = astLoop
    ( State (EMPTY) (dropUntilP ts) sl 
        (tr `makeChildren` [
                                 (astLoop (State 
                                                (Ie expr1)
                                                (takeUntilP (drop 1 ts))
                                                []
                                                (Node "<Boolean Expression>" [])
                                 )),
                                 (Node (show op) []),
                                 (astLoop (State 
                                                (Ie expr2)
                                                (takeUntilP $ drop 1 $ dropUntilP $ drop 1 $ (init ts))
                                                []
                                                (Node "<Boolean Expression>" [])
                                 ))
                           ]
        )
    ) 


--expr2 is BooleanExpr Multiple and expr1 is not
astLoop state@(State i@(Ie e@(BooleanEXPR lit@(BooleanLitM expr1 op expr2@(BooleanEXPR subLit@(BooleanLitM _ _ _))))) ts sl tr) = astLoop
    ( State (EMPTY) (dropUntilP ts) sl 
        (tr `makeChildren` [
                                 (Node (show (takeWhile ((/=(show op)).show) (drop 1 ts))) []),
                                 (Node (show op) []),
                                 (astLoop (State 
                                                (Ie expr2)
                                                (takeUntilP $ drop 1 $ (dropWhile ((/=(show op)).show) (init ts)))
                                                []
                                                (Node "<Boolean Expression>" [])
                                 ))
                           ]
        ) 
    )

--expr1 is BooleanExpr Multiple and expr2 is not
astLoop state@(State i@(Ie e@(BooleanEXPR lit@(BooleanLitM expr1@(BooleanEXPR subLit@(BooleanLitM _ _ _)) op expr2))) ts sl tr) = astLoop
    ( State (EMPTY) (dropUntilP ts) sl 
        (tr `makeChildren` [
                                 (astLoop (State 
                                                (Ie expr1)
                                                (takeUntilP (drop 1 ts))
                                                sl
                                                (Node "<Boolean Expression>" [])
                                          ) 
                                 ),
                                 (Node (show op) []),
                                 (Node (show ( takeWhile ((/=")").show) $ drop 1 $ dropUntilP $ (drop 1 ts) )) [])
                           ]
        ) 
    )

--neither expr1 or expr2 are BooleanExpr Multiple
astLoop state@(State i@(Ie e@(BooleanEXPR lit@(BooleanLitM expr1 op expr2))) ts sl tr) = astLoop
    ( State (EMPTY) (dropUntilP ts) sl 
        (tr `makeChildren` [
                                 (Node (show (takeWhile ((/=(show op)).show) (drop 1 ts))) []),
                                 (Node (show op) []),
                                 (Node (show ( takeWhile ((/=")").show) ((drop 1) $ (dropWhile ((/=(show op)).show) ts)) )) [])
                           ]
        ) 
    ) 

--STMT
--VarDecl
--LAST STATEMENT
astLoop state@(State (i@(Is (stmt@(VarDeclSTMT t id)))) ts [] tr) = astLoop 
    ( State (EMPTY) (drop 2 ts) [] ( tr `makeChild` (Node "<Variable Declaration>" [(Node (show t) []), (Node (show id) [])]) ) )
--STATEMENTS TO GO
astLoop state@(State (i@(Is (stmt@(VarDeclSTMT t id)))) ts ((n@(STMTlistNode s)):sl) tr) = astLoop 
    ( State  (Is s) (drop 2 ts) sl ( tr `makeChild` (Node "<Variable Declaration>" [(Node (show t) []), (Node (show id) [])]) ) )

--AssignStatement
--LAST STATEMENT
astLoop state@(State (i@(Is (stmt@(AssignSTMT id expr)))) ts [] tr) = 
    --if id equal to another id
    if ( ((validIdToken (ts!!2)) == True ) ) 
        then astLoop ( State (EMPTY) (drop 3 ts) [] 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (drop 1 $ init $ show (ts!!2)) [])])) )
    --if id equal to Boolean Expr Multiple
    else if ((ts!!2) == T_LParen)
        then astLoop ( State (EMPTY) (dropUntilP (drop 2 ts)) []
                        (tr `makeChild` (Node 
                                                "<Assign Statement>" 
                                                ([(Node (show id) [])]++[(astLoop (State 
                                                                                    (Ie expr)
                                                                                    (takeUntilP (drop 2 ts))
                                                                                    []
                                                                                    (Node "<Boolean Expression>" [])
                                                                                  ) 
                                                                        )]
                                                )                                            
                                        )
                        )
                     )
    --OLD: --if id equal to Boolean Expr Multiple
    --else if ((ts!!2) == T_LParen)
        --then astLoop ( State (EMPTY) (dropUntilP (drop 2 ts)) []
                        --(tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (show (takeUntilP (drop 2 ts))) [])])) )
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
--STATEMENTS TO GO
astLoop state@(State (i@(Is (stmt@(AssignSTMT id expr)))) ts ((n@(STMTlistNode s)):sl) tr) = 
    --if id equal to another id
    if ( ((validIdToken (ts!!2)) == True ) ) 
        then astLoop ( State (Is s) (drop 3 ts) sl 
                        (tr `makeChild` (Node "<Assign Statement>" [(Node (show id) []), (Node (drop 1 $ init $ show (ts!!2)) [])])) )
    --if id equal to Boolean Expr Multiple
    else if ((ts!!2) == T_LParen)
        then astLoop ( State (Is s) (dropUntilP (drop 2 ts)) []
                        (tr `makeChild` (Node 
                                                "<Assign Statement>" 
                                                ([(Node (show id) [])]++[(astLoop (State 
                                                                                    (Ie expr)
                                                                                    (takeUntilP (drop 2 ts))
                                                                                    []
                                                                                    (Node "<Boolean Expression>" [])
                                                                                  ) 
                                                                        )]
                                                )                                            
                                        )
                        )
                     )
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
--LAST STATEMENT w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(PrintSTMT expr@(BooleanEXPR lit@(BooleanLitM _ op _)))))) ts [] tr) = astLoop 
    (State (EMPTY) (dropUntilP (drop 1 ts)) [] 
                        (tr `makeChild` (Node "<Print Statement>" 
                            [(astLoop (State (Ie expr) (takeUntilP (drop 2 (init ts))) [] (Node "<Boolean Expression>" [])))])) )
--LAST STATEMENT
astLoop state@(State (i@(Is (stmt@(PrintSTMT expr)))) ts [] tr) = astLoop 
    (State (EMPTY) (dropUntilP (drop 1 ts)) [] 
                        (tr `makeChild` (Node "<Print Statement>" [(Node (show $ drop 1 $ init $ (takeUntilP (drop 1 ts))) [])])) )
--STATEMENTS TO GO w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(PrintSTMT expr@(BooleanEXPR lit@(BooleanLitM _ op _)))))) ts ((n@(STMTlistNode s)):sl) tr) = astLoop 
    (State (Is s) (dropUntilP (drop 1 ts)) sl 
                        (tr `makeChild` (Node "<Print Statement>" 
                            [(astLoop (State (Ie expr) (init $ tail $ (takeUntilP (drop 1 ts))) [] (Node "<Boolean Expression>" [])))])) )
--STATEMENTS TO GO
astLoop state@(State (i@(Is (stmt@(PrintSTMT expr)))) ts ((n@(STMTlistNode s)):sl) tr) = astLoop 
    (State (Is s) (dropUntilP (drop 1 ts)) sl 
                        (tr `makeChild` (Node "<Print Statement>" [(Node (show $ drop 1 $ init $ (takeUntilP (drop 1 ts))) [])])) )


-----------------------------------------------
--WhileStatement
--LAST STATEMENT w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr@(BooleanLitM _ op _) b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ dropUntilP (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(astLoop (State (Ie (BooleanEXPR boolExpr)) (takeUntilP (drop 1 ts)) [] (Node "<While Boolean Expression>" []))), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--LAST STATEMENT w/ SINGLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ drop 1 (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(Node "<While Boolean Expression>" [Node (last (words (show boolExpr))) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (drop 1 (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--STATEMENTS TO GO w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr@(BooleanLitM _ op _) b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ dropUntilP (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(astLoop (State (Ie (BooleanEXPR boolExpr)) (takeUntilP (drop 1 ts)) [] (Node "<While Boolean Expression>" []))), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--STATEMENTS TO GO w/ SINGLE boolExpr
astLoop state@(State (i@(Is (stmt@(WhileSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ drop 1 (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(Node "<While Boolean Expression>" [Node (last (words (show boolExpr))) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (drop 1 (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )

--------------------------------------------
--IfStatement
--LAST STATEMENT w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr@(BooleanLitM _ op _) b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ dropUntilP (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(astLoop (State (Ie (BooleanEXPR boolExpr)) (takeUntilP (drop 1 ts)) [] (Node "<If Boolean Expression>" []))), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--LAST STATEMENT w/ SINGLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts [] tr) = astLoop 
    (State 
        (EMPTY) (dropUntilB $ drop 1 (drop 1 ts)) [] 
        (tr `makeChildren` 
            [(Node "<If Boolean Expression>" [Node (last (words (show boolExpr))) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (drop 1 (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--STATEMENTS TO GO w/ MULTIPLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr@(BooleanLitM _ op _) b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ dropUntilP (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(astLoop (State (Ie (BooleanEXPR boolExpr)) (takeUntilP (drop 1 ts)) [] (Node "<If Boolean Expression>" []))), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (dropUntilP (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )
--STATEMENTS TO GO w/ SINGLE boolExpr
astLoop state@(State (i@(Is (stmt@(IfSTMT boolExpr b@(Block ((subN@(STMTlistNode subStmt)):subStmtLst)))))) ts ((n@(STMTlistNode s)):sl) tr) = 
    astLoop 
    (State 
        (Is s) (dropUntilB $ drop 1 (drop 1 ts)) sl 
        (tr `makeChildren` 
            [(Node "<If Boolean Expression>" [Node (last (words (show boolExpr))) []]), 
            (astLoop (State (Is subStmt) ((drop 1) $ init $ (takeUntilB $ (drop 1 (drop 1 ts)))) (subStmtLst) (Node "<BLOCK>" [])))]
        ) 
    )


--Statement can't be recognized
astLoop state@(State i ts sl tr) = Node "ERROR: (astLoop) pattern not reached!" []





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

takeUntilPStrLst :: [String] -> [String]
takeUntilPStrLst ss = untilP ss 0 []
    where
        untilP ((s@("(")):ss) i rss = untilP ss (i+1) (rss++[s])
        untilP ((s@(")")):ss) i rss = untilP ss (i-1) (rss++[s])
        untilP _ 0 rss = rss
        untilP (s:ss) i rss = untilP ss i (rss++[s])

dropUntilP :: [Token] -> [Token]
dropUntilP ts = untilP ts 0
    where
        untilP ((t@(T_LParen)):ts) i = untilP ts (i+1)
        untilP ((t@(T_RParen)):ts) i = untilP ts (i-1)
        untilP ts 0 = ts
        untilP (t:ts) i = untilP ts i

dropUntilPStrLst :: [String] -> [String]
dropUntilPStrLst ss = untilP ss 0
    where
        untilP ((s@("(")):ss) i = untilP ss (i+1)
        untilP ((s@(")")):ss) i = untilP ss (i-1)
        untilP ss 0 = ss
        untilP (s:ss) i = untilP ss i

takeUntilB :: [Token] -> [Token]
takeUntilB ts = untilB ts 0 []
    where
        untilB ((t@(T_LBrace)):ts) i rts = untilB ts (i+1) (rts++[t])
        untilB ((t@(T_RBrace)):ts) i rts = untilB ts (i-1) (rts++[t])
        untilB _ 0 rts = rts
        untilB (t:ts) i rts = untilB ts i (rts++[t])

takeUntilBStrLst :: [String] -> [String]
takeUntilBStrLst ss = untilB ss 0 []
    where
        untilB ((s@("{")):ss) i rss = untilB ss (i+1) (rss++[s])
        untilB ((s@("}")):ss) i rss = untilB ss (i-1) (rss++[s])
        untilB _ 0 rss = rss
        untilB (s:ss) i rss = untilB ss i (rss++[s])

dropUntilB :: [Token] -> [Token]
dropUntilB ts = untilB ts 0
    where
        untilB ((t@(T_LBrace)):ts) i = untilB ts (i+1)
        untilB ((t@(T_RBrace)):ts) i = untilB ts (i-1)
        untilB ts 0 = ts
        untilB (t:ts) i = untilB ts i

dropUntilBStrLst :: [String] -> [String]
dropUntilBStrLst ss = untilB ss 0
    where
        untilB ((s@("{")):ss) i = untilB ss (i+1)
        untilB ((s@("}")):ss) i = untilB ss (i-1)
        untilB ss 0 = ss
        untilB (s:ss) i = untilB ss i

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


--if ((tcbmHelp st tr) == "NOTSAME") then False
--                        else True
--Type Checking Functions
passTypeCheckBoolM :: SymbolTable -> Tree String -> Bool
passTypeCheckBoolM st tr =
    if( (tcbmHelp st tr) /= "NOTSAME") then True 
    else False

tcbmHelp :: SymbolTable -> Tree String -> String
tcbmHelp st tr@(Node n@("<Boolean Expression>") kids) = 
    compareAndReturnType st 
        (tcbmHelp st (kids!!0)) 
        (tcbmHelp st (kids!!2))
tcbmHelp st tr@(Node n kids) = decideType st (getVal tr)

--tcbmHelp :: SymbolTable -> Tree String -> String
--tcbmHelp st tr@(Node n@("<Boolean Expression>") kids) = 
--    compareAndReturnType st 
--        (tcbmHelp st (kids!!0)) 
--        (tcbmHelp st (kids!!2))
--tcbmHelp st tr@(Node n kids) = decideType st n


--tcbmHelp :: SymbolTable -> Tree String -> String
--tcbmHelp st tr@(Node n kids)  = 
--    if ( ("Boolean Expression" `isInfixOf` (getVal (kids!!0))) && ("Boolean Expression" `isInfixOf` (getVal (kids!!2))) )
--        then compareAndReturnType st
--                (decideType st (tcbmHelp st (kids!!0)))
--                (decideType st (tcbmHelp st (kids!!2))) 
--    else if ("Boolean Expression" `isInfixOf` (getVal (kids!!2)))
--        then compareAndReturnType st
--                (decideType st (getVal (kids!!0)))
--                (decideType st (tcbmHelp st (kids!!2)))
--    else if ("Boolean Expression" `isInfixOf` (getVal (kids!!0)))
--        then compareAndReturnType st
--                (decideType st (tcbmHelp st (kids!!0)))
--                (decideType st (getVal (kids!!2)))
--    else
--             compareAndReturnType st
--                (decideType st (getVal (kids!!0)))
--                (decideType st (getVal (kids!!2)))     


compareAndReturnType :: SymbolTable -> String -> String -> String
compareAndReturnType st a b = 
    if      (a == "NOTSAME") then "NOTSAME"
    else if (b == "NOTSAME") then "NOTSAME"
    else if ((compareType st a b) == True) then decideType st a
    else "NOTSAME"

compareType :: SymbolTable -> String -> String -> Bool
compareType st a b = 
    if      (a == "NOTSAME") then False
    else if (b == "NOTSAME") then False
    else if ((decideType st a) == (decideType st b)) then True
    else False


decideType :: SymbolTable -> String -> String
decideType st a = 
    if (a == "NOTSAME") then "NOTSAME"
    else if ( a `isElem` st ) then (getType a st)
    else if ((decideInt a st) == True) then "int"
    else if ((decideString a st) == True) then "string"
    else "boolean"
--CONDITION: MUST RUN BEFORE decideString TO PICK UP NUMBERS BEFORE LENGTH IS COUNTED!!!
decideInt :: String -> SymbolTable -> Bool
decideInt a st = 
    if ((True `notElem` (map (=='=') a)) && (("0" `isInfixOf` a) || ("1" `isInfixOf` a) || ("2" `isInfixOf` a) || ("3" `isInfixOf` a) || ("4" `isInfixOf` a) || ("5" `isInfixOf` a) || ("6" `isInfixOf` a) || ("7" `isInfixOf` a) || ("8" `isInfixOf` a) || ("9" `isInfixOf` a)) )
        then True 
    else False

decideString :: String -> SymbolTable -> Bool
decideString a st = 
    if ( ((length a) > 1) && (a /= "true") && (a /= "false") && (a /= "NOTSAME") && (True `notElem` (map (=='=') a)) && (True `notElem` (map (=='<') a)) ) 
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
makeTable tr@(Node val children) = processKids children 0 0 (SCOPE 0 []) (SymbolTable [])

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
        then error "ERROR: SCOPECHECK FAILED: [this error won't ever be reached. this is a passScopeCheck example.]"
    --SCOPECHECK ERROR FOR RHS: IF ID
    else if ( (isValidId (getVal (subKids!!1))) && 
                ( 
                    ((getVal (subKids!!1)) `isNotElem` table) || 
                    ( ((getScope (getVal (subKids!!1)) table) `inScope` (curScope `fromScope` scopeMap)) == False ) 
                ) 
            )
        then error ("ERROR: SCOPECHECK FAILED: "++(getVal (subKids!!1))++" is not in scope!!!")
    --SCOPECHECK ERROR FOR RHS: IF CONTAINS AN ID (BoolExpr Multiple and possibly others)
    else if ( 
                (containsValidId (getVal (subKids!!1))) && 
                ( False `elem` (map (passScopeCheck curScope scopeMap table) (retrieveValidIds (getVal (subKids!!1)))) ) 
            ) 
        then error "ERROR: SCOPECHECK FAILED: [this error won't ever be reached, passScopeCheck used instead]"
    --TYPECHECK ERROR FOR BOTH, LHS&RHS
    else if ( (compareType table (getVal (subKids!!0)) (getVal (subKids!!1))) == False )
        then error  ("ERROR: TYPECHECK FAILED: "++(getVal (subKids!!0))++" and "++(getVal (subKids!!1))++" are of different types!")
    --TYPECHECK ERROR FOR RHS: IF RHS IS A BOOLEANEXPR MULTIPLE [GOES HERE!!]
    --else if ( ("Boolean Expression" `isInfixOf` (getVal (subKids!!1))) && (not (passTypeCheckBoolM table (subKids!!1))) )
    --    then error "ERROR: TYPECHECK FAILED: [this error won't ever be reached, passScopeCheck used instead]*"
    --TYPECHECK ERROR FOR RHS: IF ID IN RHS ISN'T SAME TYPE AS RHS ITSELF [just for IntExpr Multiples at the moment]
    else if (
                (containsValidId (getVal (subKids!!1))) &&
                (False `elem` (map (compareType table (getVal (subKids!!1))) (retrieveValidIds (getVal (subKids!!1)))))
            )
        then error ("ERROR: TYPECHECK FAILED: Type of an ID in the right hand side isn't of proper type!")
    --PASSED SCOPECHECK AND TYPECHECK
    else
        processKids kids curScope hiScope scopeMap table

--PrintStatement
processKids (kid@(Node "<Print Statement>" subKids):kids) curScope hiScope scopeMap table = 
    --SCOPECHECK IF ID'S IN EXPR
    if ( 
            (containsValidId (getVal (subKids!!0))) && 
            ( False `elem` (map (passScopeCheck curScope scopeMap table) (retrieveValidIds (getVal (subKids!!0)))) )
       ) 
        then error "ERROR: SCOPECHECK FAILED: [this error won't ever be reached. this is a passScopeCheck example.]"
    --TYPECHECK ERROR: IF ID ISN'T SAME TYPE AS EXPR ITSELF [just for IntExpr Multiples at the moment]
    else if (
                (containsValidId (getVal (subKids!!0))) &&
                (False `elem` (map (compareType table (getVal (subKids!!0))) (retrieveValidIds (getVal (subKids!!0)))))
            )
        then error ("ERROR: TYPECHECK FAILED: ID isn't of proper type!")
    --PASSED SCOPECHECK AND TYPECHECK
    else
        processKids kids curScope hiScope scopeMap table


--PATTERN NOT REACHED
processKids _ _ _ _ _ = error "ERROR: Pattern not reached in processKids!!!"
    





--Id Identification helpers
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



--creates a sample table
testTable :: SymbolTable
testTable = SymbolTable [ 
                            (IDRow "a" "int"        0), 
                            (IDRow "b" "boolean"    0), 
                            (IDRow "c" "string"     1),
                            (IDRow "d" "int"        2) 
                        ]

emptyTable :: SymbolTable
emptyTable = SymbolTable []


testScope0 :: SCOPE
testScope0 = (SCOPE 0 [(SCOPE 1 [(SCOPE 2 [])])])

testScope1 :: SCOPE
testScope1 = (SCOPE 1 [(SCOPE 2 [])])

testScope2 :: SCOPE
testScope2 = (SCOPE 2 [])


testTree :: Tree String
testTree = Node "<Boolean Expression>" [(Node "5" []), (Node "==" []), (Node "true" [])]








    






