module Parser where

import LanguageData
import Grammar
import Data.Tree


import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)
import Text.ParserCombinators.ReadP (look)
import Text.Parsec (parse)

import Control.Applicative
import Control.Monad


--GHCI test functions
--to use: enter ghci main in your Unix command line. from there, enter any of these functions:

parseTEST = (parse programP "test" "{print 5} $")
parseIntTEST'  = (parse programP "IntM parse test" "{print 7 + 2 + 5} $")
parseIntTEST  = (parse programP "IntM parse test" "{print7+2+5}$")
parseBoolTEST = (parse programP "Bool parse test" "{print(5 == 5)}$")
parseBoolTEST' = (parse programP "Bool parse test" "{print true}$")
parseStrTEST = (parse programP "String parse test" ("{print "++('"':"abc")++('"':"}$")))
parseDeclTEST = (parse programP "Var Decl test" "{inta}$")
parseAssnTEST = (parse programP "Assign Statement test" "{x=5}$")
parseWhileTEST = (parse programP "While Statement test" "{whiletrue{print5}}$")
parseIfTEST = (parse programP "If Statement test" "{iftrue{print5}}$")







-------PARSER START-------

--testP :: Parser (Tree String)
--testP = spaceP *> boolValP



programP :: Parser PROGRAM
programP = (Program <$> blockP) <* skipSpaces <* (string "$")
--

blockP :: Parser BLOCK
blockP = (string "{") *> skipSpaces *> (Block <$> stmtListP) <* skipSpaces <* (string "}")
--

stmtListP :: Parser [STMTlist]
stmtListP = many (STMTlistNode <$> stmtP)
--

--WARNING: make sure AssignSTMT is last!!! idP/charP will pick up any char.

stmtP :: Parser STMT
stmtP = ((string "i") *> (
                                (string "f" *> skipSpaces *> (IfSTMT <$> (booleanExprLitP <*skipSpaces) <*> blockP)) 
                            <|> (string "nt" *> skipSpaces *> ( VarDeclSTMT <$> (pure INT) <*> idP ))
                            <|> ( AssignSTMT <$> ((Id<$>(pure I)) <* skipSpaces <* (string "=") <* skipSpaces) <*> exprP )
                         ) <* skipSpaces)
    <|> ((string "b") *> (
                                (string "oolean" *> skipSpaces *> ( VarDeclSTMT <$> (pure BOOLEAN) <*> idP ))
                            <|> ( AssignSTMT <$> ((Id<$>(pure B)) <* skipSpaces <* (string "=") <* skipSpaces) <*> exprP )
                         ) <* skipSpaces )
    <|> ((string "s") *> (
                                (string "tring" *> skipSpaces *> ( VarDeclSTMT <$> (pure STRING) <*> idP ))
                            <|> ( AssignSTMT <$> ((Id<$>(pure S)) <* skipSpaces <* (string "=") <* skipSpaces) <*> exprP )
                         ) <* skipSpaces )
    <|> ((string "p") *> (
                                (string "rint" *> skipSpaces *> (string "(") *> skipSpaces *> (PrintSTMT <$> exprP) <* skipSpaces <* (string ")") )
                            <|> ( AssignSTMT <$> ((Id<$>(pure P)) <* skipSpaces <* (string "=") <* skipSpaces) <*> exprP )
                         ) <* skipSpaces)
    <|> ((string "w") *> (
                                (string "hile" *> skipSpaces *> (WhileSTMT <$> (booleanExprLitP <*skipSpaces) <*> blockP) )
                            <|> ( AssignSTMT <$> ((Id<$>(pure W)) <* skipSpaces <* (string "=") <* skipSpaces) <*> exprP )
                         ) <* skipSpaces)
    <|> (( AssignSTMT <$> (idP <* skipSpaces <* (string "=") <* skipSpaces) <*> exprP )     <* skipSpaces)
--

--old, broken version:
--stmtP :: Parser STMT
--stmtP = (( (string "print") *> skipSpaces *> (string "(") *> skipSpaces *> (PrintSTMT <$> exprP) <* skipSpaces <* (string ")") )   <* skipSpaces)
--w    <|> (( (string "while") *> skipSpaces *> (WhileSTMT <$> (booleanExprLitP <*skipSpaces) <*> blockP) )   <* skipSpaces)
--w    <|> (( VarDeclSTMT <$> typeP <*> idP )    <* skipSpaces)
--w    <|> (( (string "if") *> skipSpaces *> (IfSTMT <$> (booleanExprLitP <*skipSpaces) <*> blockP) )         <* skipSpaces)
--w    <|> (( AssignSTMT <$> (idP <* skipSpaces <* (string "=") <* skipSpaces) <*> exprP )     <* skipSpaces)

--BOOLEAN EXPRESSION WAS FIRST
exprP :: Parser EXPR
exprP = ( (StringEXPR <$> stringExprLitP)   <* skipSpaces)
    <|> ( (BooleanEXPR <$> booleanExprLitP) <* skipSpaces)
    <|> ( (IntEXPR <$> intExprLitP)         <* skipSpaces)
    <|> ( (IDEXPR <$> idP)                  <* skipSpaces)
--

intExprLitP :: Parser IntEXPRlit
intExprLitP = (IntLitS <$> digitP) <* skipSpaces <* (optional (intOpP <* (exprP `sepBy` intOpP))) <* skipSpaces
--intExprLitP = (IntLitM <$> digitP <*> intOpP <*> exprP)
--          <|> (IntLitS <$> digitP)
--

stringExprLitP :: Parser StringEXPRlit
stringExprLitP = ( ((char '"') *> (StringLit <$> charListP) <* (char '"'))          <* skipSpaces)
             <|> ( ((string "\"") *> (StringLit <$> charListP) <* (string "\""))    <* skipSpaces)

--

booleanExprLitP :: Parser BooleanEXPRlit
booleanExprLitP = ((string "(") *> skipSpaces *> (BooleanLitM <$> exprP <*> boolOpP <*> exprP) <* skipSpaces <* (string ")") )
              <|> ( BooleanLitS <$> boolValP ) 
--

--WARNING: this shouldn't matter, but be careful of skipSpace here!
idP :: Parser ID
idP = (Id <$> charP)
--

charListP :: Parser [CHARlist]
charListP = many ((CHARlistNodeS <$> spaceP) <|> (CHARlistNodeC <$> charP))
--

typeP :: Parser TYPE
typeP = ((string "int") *> skipSpaces *> pure INT)
    <|> ((string "string") *> skipSpaces *> pure STRING)
    <|> ((string "boolean") *> skipSpaces *> pure BOOLEAN)
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
boolOpP = (((string "==") *> skipSpaces *> pure EQUALS) <* skipSpaces)
      <|> (((string "!=") *> skipSpaces *> pure NOTEQUALS) <* skipSpaces)
--

--already has skipSpaces in boolValP
boolFalseP :: Parser BOOLVAL
boolFalseP = (string (show FALSE)) *> skipSpaces *> pure FALSE

boolTrueP :: Parser BOOLVAL
boolTrueP = (string (show TRUE)) *> skipSpaces *> pure TRUE

boolValP :: Parser BOOLVAL
boolValP = (boolTrueP <|> boolFalseP) <* skipSpaces
--

intOpP :: Parser INTOP
intOpP = ((string (show INTOP)) *> skipSpaces *> pure INTOP) <* skipSpaces
--

skipSpaces = ( skipMany ((string " ")<|>(string "\n")<|>(string "\t")) )
--skipSpaces = (skipMany (string " "))
--

-------PARSER STOP-------












