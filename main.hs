import Lexer


main :: IO ()
main = do 
	message
	print (makeTOKEN ")")

	--TEST CODE FOR STORING VARIABLES&INPUT
	--userInput <- getLine
	--handle userInput


message :: IO ()
message = do putStrLn "Handling Token..."


--tokenize :: [Char] -> [TOKEN]



makeTOKEN :: [Char] -> TOKEN
makeTOKEN "(" = T_LParen
makeTOKEN ")" = T_RParen
makeTOKEN x = Invalid
