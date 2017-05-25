module Grammar where


import LanguageData
import Lexer


data PROGRAM = Program              BLOCK           deriving (Eq, Show)

data BLOCK = Block                  STMTlist        deriving (Eq, Show)

data STMTlist = STMTlistNode        STMT STMTlist 
              | EmptySTMTlist
                                                    deriving (Eq, Show)

data STMT = PrintSTMT               EXPR
          | AssignSTMT              ID EXPR
          | VarDeclSTMT             TYPE ID
          | WhileSTMT               BooleanEXPRlit BLOCK
          | IfSTMT                  BooleanEXPRlit BLOCK
                                                    deriving (Eq, Show)




data EXPR = IntEXPR                 IntEXPRlit
          | StringEXPR              StringEXPRlit
          | BooleanEXPR             BooleanEXPRlit
          | IDEXPR                  ID
                                                    deriving (Eq, Show)

data IntEXPRlit = IntEXPRlitM       DIGIT INTOP EXPR
                | IntEXPRlitS       DIGIT
                                                    deriving (Eq, Show)

data StringEXPRlit = StringLit      CHARlist        deriving (Eq, Show)


data CHARlist = CHARlistNode        CHAR CHARlist   
              | EmptyCHARlist
                                                    deriving (Eq, Show)

data BooleanEXPRlit = BE Int                        deriving (Eq, Show)



data ID = Id                        CHAR            deriving (Eq, Show)

data TYPE = INT
          | STRING
          | BOOLEAN
                                                    deriving (Eq, Show)

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
                                                    deriving (Eq, Show)

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
                                                    deriving (Eq, Show)

data BOOLOP = EQUALS
            | NOTEQUALS
                                                    deriving (Eq, Show)

data BOOLVAL = TRUE
             | FALSE
                                                    deriving (Eq, Show)

data INTOP = INTOP                                  deriving (Eq, Show)



