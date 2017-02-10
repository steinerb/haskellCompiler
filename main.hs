import Lexer


main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    print (tokenize userInput)
    --print (tokenizeHelp (newState "("))

introMessage :: IO ()
introMessage = do putStrLn "Enter a String to be tokenized:"


