module Grammar where

import Lexer

data Program = Program Block Token

data Block = Block Token StatementList Token

--WILL BE ADDED TO; just need to have something compiling for now
data StatementList = Statement

--IS NOT INT
data Statement = Int