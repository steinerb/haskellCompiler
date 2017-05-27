module Parser where

import LanguageData
import Grammar
import Data.Tree


import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)
import Text.Parsec (parse)

import Control.Applicative
import Control.Monad


--GHCI test functions
--to use: enter ghci main in your Unix command line. from there, enter any of these functions:

parseTEST = (parse programP "Var Decl test" "{inta}$")
parseIntTEST  = (parse programP "IntM parse test" "{print7 + 2 + 5}$")
parseIntTEST'  = (parse programP "IntM parse test" "{print7+2+5}$")
parseBoolTEST = (parse programP "Bool parse test" "{print(5==5)}$")
parseBoolTEST' = (parse programP "Bool parse test" "{printtrue}$")
parseDeclTEST = (parse programP "Var Decl test" "{inta}$")
parseAssnTEST = (parse programP "Assign Statement test" "{x=5}$")





-------PARSER START-------

--testP :: Parser (Tree String)
--testP = spaceP *> boolValP



programP :: Parser PROGRAM
programP = (Program <$> blockP) <* skipSpaces <* (string "$")


blockP :: Parser BLOCK
blockP = (string "{") *> (Block <$> stmtListP) <* (string "}")

stmtListP :: Parser [STMTlist]
stmtListP = many (STMTlistNode <$> stmtP)

--stmtP INCOMPLETE!!! (only includes print statements currently)
--WARNING: make sure AssignSTMT is last!!! idP/charP will pick up any char.
stmtP :: Parser STMT
stmtP = ( (string "print") *> (PrintSTMT <$> exprP) )
    <|> ( VarDeclSTMT <$> typeP <*> idP )
    <|> ( AssignSTMT <$> (idP <* (string "=")) <*> exprP )
--

exprP :: Parser EXPR
exprP = (IntEXPR <$> intExprLitP)
    <|> (StringEXPR <$> stringExprLitP)
    <|> (BooleanEXPR <$> booleanExprLitP) 
    <|> (IDEXPR <$> idP)
--

intExprLitP :: Parser IntEXPRlit
intExprLitP = (IntLitS <$> digitP) <* (optional (intOpP <* (exprP `sepBy` intOpP)))
--

stringExprLitP :: Parser StringEXPRlit
stringExprLitP = (string "\"") *> (StringLit <$> charListP) <* (string "\"")
--

--OLD WORKING VERSION WITHOUT <*>!!
--booleanExprLitP :: Parser BooleanEXPRlit
--booleanExprLitP = ((string "(") *> (exprP) *> (boolOpP) *> (exprP) *> (string ")") *> (pass) )
--t              <|> ( BooleanLitS <$> boolValP ) 
--t                    where
--t                        pass = pure (BooleanLitS TRUE)

booleanExprLitP :: Parser BooleanEXPRlit
booleanExprLitP = ((string "(") *> (BooleanLitM <$> exprP <*> boolOpP <*> exprP) <* (string ")") )
              <|> ( BooleanLitS <$> boolValP ) 
--

--WARNING: this shouldn't matter, but be careful of skipSpace here!
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
boolOpP = (skipSpaces *> (string "==") *> skipSpaces *> pure EQUALS)
      <|> (skipSpaces *> (string "!=") *> skipSpaces *> pure NOTEQUALS)
--

boolFalseP :: Parser BOOLVAL
boolFalseP = skipSpaces *> (string (show FALSE)) *> skipSpaces *> pure FALSE

boolTrueP :: Parser BOOLVAL
boolTrueP = skipSpaces *> (string (show TRUE)) *> skipSpaces *> pure TRUE

boolValP :: Parser BOOLVAL
boolValP = skipSpaces *> (boolTrueP <|> boolFalseP) <* skipSpaces
--

intOpP :: Parser INTOP
intOpP = skipSpaces *> (string (show INTOP)) *> skipSpaces *> pure INTOP
--

skipSpaces = (skipMany (string " "))
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





