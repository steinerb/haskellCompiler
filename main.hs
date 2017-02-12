import Lexer
import Grammar

main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    print (map (tokenize) (splitByEOP userInput []))
    --print (tokenizeHelp (newState "("))

introMessage :: IO ()
introMessage = do putStrLn "Enter a String to be tokenized (any combination of: {, }, int, $):"

--breaks programs up by $'s
splitByEOP :: String -> [String] -> [String]
splitByEOP ((fst@('$')):i) lst = splitByEOP i lst
splitByEOP [] lst = lst
splitByEOP i lst = splitByEOP (dropWhile (/='$') i) (lst++[(takeWhile (/='$') i)])