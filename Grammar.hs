module Grammar where



--import Lexer
import Data.Char (toLower)



data PROGRAM = Program              BLOCK           deriving (Eq, Show)


data BLOCK = Block                  [STMTlist]        
                                                    deriving (Eq, Show)


data STMTlist = STMTlistNode        STMT
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

data IntEXPRlit = IntLitM           DIGIT INTOP EXPR
                | IntLitS           DIGIT
                                                    deriving (Eq, Show)

data StringEXPRlit = StringLit      [CHARlist]      deriving (Eq, Show)



data CHARlist = CHARlistNodeC       CHAR
              | CHARlistNodeS       SPACE
                                                    deriving (Eq)


data BooleanEXPRlit = BooleanLitM   EXPR BOOLOP EXPR 
                    | BooleanLitS   BOOLVAL
                                                    deriving (Eq, Show)


--MAKE DERIVING SHOW IF NO INSTANCE!
data ID = Id                        CHAR            deriving (Eq)


data TYPE = INT
          | STRING
          | BOOLEAN
                                                    deriving (Eq)

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
                                                    deriving (Eq)

data SPACE = SPACE                                  deriving (Eq)

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
                                                    deriving (Eq)

data BOOLOP = EQUALS
            | NOTEQUALS
                                                    deriving (Eq)

data BOOLVAL = TRUE
             | FALSE
                                                    deriving (Eq)

data INTOP = INTOP                                  deriving (Eq)

idToStr :: ID -> String
idToStr id@(Id c) = show c

--remove this for more accurate show at the cost of Stack Overflow
instance Show CHARlist where
    show cl@(CHARlistNodeS _) = " "
    show cl@(CHARlistNodeC c) = show c

--remove this for more accurate show at the cost of Stack Overflow
instance Show ID where
    show id@(Id a) = (show a)

instance Show TYPE where 
    show s 
            | s == INT = "int"
            | s == STRING = "string"
            | s == BOOLEAN = "boolean"

instance Show CHAR where
    --show c = show $ toLower $ head $ show c
    show c
            | c == A = "a"
            | c == B = "b"
            | c == C = "c"
            | c == D = "d"
            | c == E = "e"
            | c == F = "f"
            | c == G = "g"
            | c == H = "h"
            | c == I = "i"
            | c == J = "j"
            | c == K = "k"
            | c == L = "l"
            | c == M = "m"
            | c == N = "n"
            | c == O = "o"
            | c == P = "p"
            | c == Q = "q"
            | c == R = "r"
            | c == S = "s"
            | c == T = "t"
            | c == U = "u"
            | c == V = "v"
            | c == W = "w"
            | c == X = "x"
            | c == Y = "y"
            | c == Z = "z"







instance Show SPACE where
    show s = show " "

instance Show DIGIT where
    show d
            | d == ZERO = "0"
            | d == ONE = "1"
            | d == TWO = "2"
            | d == THREE = "3"
            | d == FOUR = "4"
            | d == FIVE = "5"
            | d == SIX = "6"
            | d == SEVEN = "7"
            | d == EIGHT = "8"
            | d == NINE = "9"

instance Show BOOLOP where
    show bo
            | bo == EQUALS = "=="
            | bo == NOTEQUALS = "!="

instance Show BOOLVAL where
    show bv
            | bv == TRUE = "true"
            | bv == FALSE = "false"

instance Show INTOP where
    show io = "+"






