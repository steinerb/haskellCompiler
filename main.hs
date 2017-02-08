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
