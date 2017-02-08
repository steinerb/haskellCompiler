import Lexer


main :: IO ()
main = do 
	message
	userInput <- getLine
	putStrLn (tokenizeHelp (makeState userInput))

	--TEST CODE FOR STORING VARIABLES&INPUT
	--userInput <- getLine
	--handle userInput


message :: IO ()
message = do putStrLn "Enter a String to be tokenized:"
