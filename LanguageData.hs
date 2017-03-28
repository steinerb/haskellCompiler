module LanguageData where

data BoolOp = BoolOp {isEq :: Bool} deriving (Eq, Show)


data Type = TypeInt
          | TypeStr
          | TypeBool
            deriving (Eq, Show)


--more tokens to be added
data Token = T_id String
           | T_LBrace
           | T_RBrace
           | T_LParen
           | T_RParen
           | T_intOp
           | T_assign
           | T_boolOp BoolOp
           | T_true
           | T_false
           | T_string String
           | T_int String
           | T_if
           | T_while
           | T_print
           | T_type Type
           | T_EOP
           | Invalid
                deriving (Eq, Show)


data State = State String String [Token] Int Int Int deriving (Show)


--CONDITION: cannot have multiple characters per number
symbolTable :: [(Int, Char)]
symbolTable =
    [
        (1,  '{'),
        (2,  '}'),
        (3,  '('),
        (4,  ')'),
        (5,  '+'),
        (6,  '!'),
        (7,  '='),
        (8,  '='),
        (9,  'w'),
        (10, 'h'),
        (11, 'i'),
        (12, 'l'),
        (13, 'e'),
        (14, 'i'),
        (15, 'f'),
        (16, 'n'),
        (17, 't'),
        (18, 'p'),
        (19, 'r'),
        (20, 'i'),
        (21, 'n'),
        (22, 't'),
        (23, 's'),
        (24, 't'),
        (25, 'r'),
        (26, 'i'),
        (27, 'n'),
        (28, 'g'),
        (29, 'b'),
        (30, 'o'),
        (31, 'o'),
        (32, 'l'),
        (33, 'e'),
        (34, 'a'),
        (35, 'n'),
        (36, 'f'),
        (37, 'a'),
        (38, 'l'),
        (39, 's'),
        (40, 'e'),
        (41, 't'),
        (42, 'r'),
        (43, 'u'),
        (44, 'e'),
        (45, '\"'),
        (46, '\"')
    ]

validLiterals :: [String]
validLiterals = ["{", "}", "(", ")", "+", "=", "!=", "==", "if", "while", "print", "int", "string", "boolean", "true", "false"]

validCharsS :: [[Char]]
validCharsS = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

validCharsC :: [Char]
validCharsC = "abcdefghijklmnopqrstuvwxyz"

validDigitsS :: [String]
validDigitsS = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

--state 8: (!|=) -> =
--TO COMPLETE FOR NOW: if, while, print, int,
stateCons :: [[Int]]
stateCons =
    --    {   }   (   )   +   !   =   i   f   w   h   l   e   p   r   n   t   s   g   b   o   a   u   "
    [
        [ 1,  2,  3,  4,  5,  6,  7,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 23, -1, 28, -1, -1, -1, -1  ], -- 0: 
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], -- 1: { found
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], -- 2: } found
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], -- 3: ( found
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], -- 4: ) found
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], -- 5: + found
        [-1, -1, -1, -1, -1,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], -- 6: ! found NOT EQ
        [-1, -1, -1, -1, -1, -1,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], -- 7: = found EQ
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], -- 8: = found END OF NOT EQ & EQ
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], -- 9: w found WHILE
        [-1, -1, -1, -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --10: h found WHILE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --11: i found WHILE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --12: l found WHILE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --13: e found WHILE
        [-1, -1, -1, -1, -1, -1, -1, -1, 15, -1, -1, -1, -1, -1, -1, 16, -1, -1, -1, -1, -1, -1, -1, -1  ], --14: i found IF, INT
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --15: f found IF
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 17, -1, -1, -1, -1, -1, -1, -1  ], --16: n found INT
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 19, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --18: p found PRINT
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 20, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --19: r found PRINT
        [-1, -1, -1, -1, -1, -1, -1, 21, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --20: i found PRINT
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 22, -1, -1, -1, -1, -1, -1, -1  ], --21: n found PRINT     
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --22: t found PRINT  
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, -1, -1, -1  ], --23: s found STRING
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --24: t found STRING
        [-1, -1, -1, -1, -1, -1, -1, 26, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --25: r found STRING
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, -1, -1, -1  ], --26: i found STRING *
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 28, -1, -1, -1, -1, -1  ], --27: n found STRING *
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --28: g found STRING
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 30, -1, -1, -1  ], --29: b found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, -1, -1  ], --30: o found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --31: o found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --32: l found BOOLEAN *
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 34, -1, -1  ], --33: e found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 35, -1, -1, -1, -1, -1, -1, -1, -1  ], --34: a found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --35: n found BOOLEAN
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 37, -1, -1  ], --36: f found FALSE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 38, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --37: a found FALSE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 39, -1, -1, -1, -1, -1, -1  ], --38: l found FALSE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 40, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --39: s found FALSE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --40: e found FALSE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 42, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --41: t found TRUE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 43, -1  ], --42: r found TRUE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 44, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --43: u found TRUE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ], --44: e found TRUE
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 46  ], --45: " found "..."
        [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  ]  --46: " found "..."
    ]
