module Grammar where


import LanguageData
import Lexer


data PROGRAM = Program              BLOCK

data BLOCK = Block                  STMTlist

data STMTlist = SList               STMT STMTlist
              | EmptySTMT

data STMT = PrintSTMT               EXPR
          | AssignSTMT              ID EXPR
          | VarDeclSTMT             TYPE ID
          | WhileSTMT               BooleanEXPRlit BLOCK
          | IfSTMT                  BooleanEXPRlit BLOCK




data EXPR = IntEXPR                 IntEXPRlit
          | StringEXPR              StringEXPRlit
          | BooleanEXPR             BooleanEXPRlit
          | IDEXPR                  ID

data IntEXPRlit = IntEXPRlitM       DIGIT INTOP EXPR
                | IntEXPRlitS       DIGIT

data StringEXPRlit = SE Int

data BooleanEXPRlit = BE Int



data ID = Id                        CHAR

data TYPE = INT
          | STRING
          | BOOLEAN

data CHAR = A
          | B
          | C
          | D
          | E
          | F
          | G
          | H
          | I
          | J
          | K
          | L
          | M
          | N
          | O
          | P
          | Q
          | R
          | S
          | T
          | U
          | V
          | W
          | X
          | Y
          | Z

data DIGIT = ZERO
           | ONE
           | TWO
           | THREE
           | FOUR
           | FIVE
           | SIX
           | SEVEN
           | EIGHT
           | NINE

data BOOLOP = EQUALS
            | NOTEQUALS

data BOOLVAL = TRUE
             | FALSE

data INTOP = INTOP



