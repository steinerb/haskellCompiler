{
module Main where

import Data.Char
import LanguageData
import Lexer

--    let             { TokenLet }
--    in              { TokenIn }
--    var             { TokenVar $$ }
}

%name calc
%tokentype { Token }
%error { parseError }


%token 
      

      id              { T_id $$ }
      '{'             { T_LBrace }
      '}'             { T_RBrace }
      '('             { T_LParen }
      ')'             { T_RParen }
      '+'             { T_intOp }
      '='             { T_assign }
      '$'             { T_EOP }


      "=="            { T_boolOp $$ }
      "!="            { T_boolOp $$ }

      true            { T_true }
      false           { T_false }

      '"'             { T_string $$ }

      digit           { T_int $$ }

      if              { T_if }
      while           { T_while }
      print           { T_print } 


      int             { T_Type $$ }
      boolean         { T_Type $$ }
      string          { T_Type $$ }
      
      

      
      
      



%%

Program                 : Block '$'                         { Program $1 }

Block                   : '{' StatementList '}'             { Block $2 }


StatementList           : Statement StatementList           { StatementList $1 $2 }

Statement               : PrintStatement                    
                        | AssignStatement
                        | VarDecl
                        | WhileStatement
                        | IfStatement
                        | Block

PrintStatement          : print '(' Expr ')'                { PrintStatement $3 }

AssignStatement         : id '=' Expr                       { AssignStatement $1 $3 }

VarDecl                 : type id                           { VarDecl $2 }

WhileStatement          : while BooleanExpr Block           { WhileStatement $2 $3 }

IfStatement             : if BooleanExpr Block              { IfStatement $2 $3 }

Expr                    : IntExpr                                                  
                        | StringExpr
                        | BooleanExpr
                        | id

IntExpr                 : digit intOp Expr                  { IntExpr $3 }
                        | digit                             

StringExpr              : '"' CharList '"'                  

BooleanExpr             : '(' Expr boolOp Expr ')'          
                        | boolVal

id                      : char

charList                : char charList
                        | space charList


char                    : 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'

space                   : ' '

digit                   : '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'

boolOp                  : ==
                        | !=

boolVal                 : false
                        | true

intOp                   : '+'

type                    : int
                        | string
                        | boolean









Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
      | Exp1                    { Exp1 $1 }

Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor                    
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }



{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp  
      = Let String Exp Exp
      | Exp1 Exp1
      deriving Show

data Exp1 
      = Plus Exp1 Term 
      | Minus Exp1 Term 
      | Term Term
      deriving Show

data Term 
      = Times Term Factor 
      | Div Term Factor 
      | Factor Factor
      deriving Show

data Factor 
      = Int Int 
      | Var String 
      | Brack Exp
      deriving Show


--***MAKE DATATYPES HERE FOR ABOVE CODE!!!***

type ID = Char


data Program = Block

data Block = StatementList


data StatementList = Statement StatementList

data Statement = PrintStatement Expr
               | AssignStatement ID Expr
               | VarDecl ID 
               | WhileStatement Expr Block
               | IfStatement Expr Block
               | Block

data Expr = IntExpr Expr
          | StringExpr String
          | BooleanExpr
          | ID --id

data BooleanExpr = Expr Expr
                 | True
                 | False






--TAKING FROM LanguageData.hs instead!!!
--data Token
--      = TokenLet
--      | TokenIn
--      | TokenInt Int
--      | TokenVar String
--      | TokenEq
--      | TokenPlus
--      | TokenMinus
--      | TokenTimes
--      | TokenDiv
--      | TokenOB
--      | TokenCB
-- deriving Show



--OLD LEXER

--lexer :: String -> [Token]
--lexer [] = []
--lexer (c:cs) 
--      | isSpace c = lexer cs
--      | isAlpha c = lexVar (c:cs)
--      | isDigit c = lexNum (c:cs)
--lexer ('=':cs) = TokenEq : lexer cs
--lexer ('+':cs) = TokenPlus : lexer cs
--lexer ('-':cs) = TokenMinus : lexer cs
--lexer ('*':cs) = TokenTimes : lexer cs
--lexer ('/':cs) = TokenDiv : lexer cs
--lexer ('(':cs) = TokenOB : lexer cs
--lexer (')':cs) = TokenCB : lexer cs

--lexNum cs = TokenInt (read num) : lexer rest
--      where (num,rest) = span isDigit cs

--lexVar cs =
--   case span isAlpha cs of
--      ("let",rest) -> TokenLet : lexer rest
--      ("in",rest)  -> TokenIn : lexer rest
--      (var,rest)   -> TokenVar var : lexer rest

main = getLine >>= print . calc . tokenize
}