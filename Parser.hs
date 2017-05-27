module Parser where

import LanguageData
import Grammar
import Data.Tree


import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)
import Text.Parsec (parse)

import Control.Applicative
import Control.Monad



parseIntTEST  = (parse programP "IntM parse test" "{print7+2+5}$")
parseBoolTEST = (parse programP "Bool parse test" "{()}$")





-------PARSER START-------

--testP :: Parser (Tree String)
--testP = spaceP *> boolValP



programP :: Parser PROGRAM
programP = (Program <$> blockP) <* (string "$")


blockP :: Parser BLOCK
blockP = (string "{") *> (Block <$> stmtListP) <* (string "}")

stmtListP :: Parser [STMTlist]
stmtListP = many (STMTlistNode <$> stmtP)

--stmtP INCOMPLETE!!! (only includes print statements currently)
stmtP :: Parser STMT
stmtP = (string "print") *> (PrintSTMT <$> exprP)
--

exprP :: Parser EXPR
exprP = (IntEXPR <$> intExprLitP)
    <|> (StringEXPR <$> stringExprLitP)
    <|> (BooleanEXPR <$> booleanExprLitP) 
    <|> (IDEXPR <$> idP)
--

--attempt 2:
--intExprLitP :: Parser IntEXPRlit 
--intExprLitP = ( (digitP *> (string "+") *> exprP *> (optional (exprP `sepBy` (string "+")))) *> pass )
--a2          <|> (IntLitS <$> digitP)
--a2                where
--a2                    pass = pure (IntLitS ZERO)
--
intExprLitP :: Parser IntEXPRlit
intExprLitP = (IntLitS <$> digitP) <* (optional ((string "+") <* (exprP `sepBy` (string "+")))) 
--

stringExprLitP :: Parser StringEXPRlit
stringExprLitP = (string "\"") *> (StringLit <$> charListP) <* (string "\"")
--

booleanExprLitP :: Parser BooleanEXPRlit
booleanExprLitP = ( ((exprP) *> (boolOpP) *> (exprP)) *> (pass) )
              <|> ( BooleanLitS <$> (boolValP) ) 
                    where
                        pass = pure (BooleanLitS TRUE)
--

idP :: Parser ID
idP = Id <$> charP
--

charListP :: Parser [CHARlist]
charListP = many ((CHARlistNodeS <$> spaceP) <|> (CHARlistNodeC <$> charP))
--

typeP :: Parser TYPE
typeP = ((string "int") *> pure INT)
    <|> ((string "string") *> pure STRING)
    <|> ((string "boolean") *> pure BOOLEAN)
--

charP :: Parser CHAR
charP = ((string "a") *> pure A)
     <|>((string "b") *> pure B)
     <|>((string "c") *> pure C)
     <|>((string "d") *> pure D)
     <|>((string "e") *> pure E)
     <|>((string "f") *> pure F)
     <|>((string "g") *> pure G)
     <|>((string "h") *> pure H)
     <|>((string "i") *> pure I)
     <|>((string "j") *> pure J)
     <|>((string "k") *> pure K)
     <|>((string "l") *> pure L)
     <|>((string "m") *> pure M)
     <|>((string "n") *> pure N)
     <|>((string "o") *> pure O)
     <|>((string "p") *> pure P)
     <|>((string "q") *> pure Q)
     <|>((string "r") *> pure R)
     <|>((string "s") *> pure S)
     <|>((string "t") *> pure T)
     <|>((string "u") *> pure U)
     <|>((string "v") *> pure V)
     <|>((string "w") *> pure W)
     <|>((string "x") *> pure X)
     <|>((string "y") *> pure Y)
     <|>((string "z") *> pure Z)
--

spaceP :: Parser SPACE
spaceP = (string " ") *> pure SPACE
--

digitP :: Parser DIGIT
digitP = ((string "0") *> pure ZERO)
     <|> ((string "1") *> pure ONE)
     <|> ((string "2") *> pure TWO)
     <|> ((string "3") *> pure THREE)
     <|> ((string "4") *> pure FOUR)
     <|> ((string "5") *> pure FIVE)
     <|> ((string "6") *> pure SIX)
     <|> ((string "7") *> pure SEVEN)
     <|> ((string "8") *> pure EIGHT)
     <|> ((string "9") *> pure NINE)
--     

boolOpP :: Parser BOOLOP
boolOpP = ((string "==") *> pure EQUALS)
      <|> ((string "!=") *> pure NOTEQUALS)
--

boolFalseP :: Parser BOOLVAL
boolFalseP = (string (show FALSE)) *> pure FALSE

boolTrueP :: Parser BOOLVAL
boolTrueP = (string (show TRUE)) *> pure TRUE

boolValP :: Parser BOOLVAL
boolValP = boolTrueP <|> boolFalseP
--


intOpP :: Parser INTOP
intOpP = (string (show INTOP)) *> pure INTOP
--



-------PARSER STOP-------




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





