import Lexer


main :: IO ()
main = do 
	message
	print (tokenize "(")

	--TEST CODE FOR STORING VARIABLES&INPUT
	--userInput <- getLine
	--handle userInput


message :: IO ()
message = do putStrLn "Lexing..."


tokenize :: [Char] -> [TOKEN]
tokenize "" = []
tokenize x = tokenizeHelper x "" []

tokenizeHelper :: [Char] -> [Char] -> [TOKEN] -> [TOKEN]
tokenizeHelper "" _ tokens = tokens
tokenizeHelper (fst:inpt) buff tkns = 
	if (elem [fst] tokens) then tokenizeHelper inpt buff tkns++[(makeTOKEN [fst])]
	else error "ERROR"




makeTOKEN :: [Char] -> TOKEN
makeTOKEN "(" = T_LParen
makeTOKEN ")" = T_RParen
makeTOKEN x = Invalid
