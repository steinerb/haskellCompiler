import Lexer


main :: IO ()
main = do 
	message
	print (handleTOKEN T_RParen)

	--userInput <- getLine
	--handle userInput

--OUTPUT MESSAGES
message :: IO ()
message = do putStrLn "Handling Token..."

--HANDLERS
handleTOKEN :: TOKEN -> [Char]
handleTOKEN x@(T_RParen) = "}"
handleTOKEN x = "other..."
