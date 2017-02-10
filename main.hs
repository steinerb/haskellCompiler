import Lexer


main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    --tokens <- (tokenize userInput)
    --programs <- (tokensToPrograms tokens)
    print (tokenize userInput)
    --print (tokenizeHelp (newState "("))

introMessage :: IO ()
introMessage = do putStrLn "Enter a String to be tokenized (any combination of: {, }, int, $):"
