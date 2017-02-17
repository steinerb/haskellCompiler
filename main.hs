import Lexer
import Grammar

main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    programStrings <- return (map (tokenize) (splitByEOP userInput []))
    print programStrings

introMessage :: IO ()
introMessage = do putStrLn "Enter a String to be tokenized!\nWorking Tokens Include: {, }, (, ), +, ==, !=, while"

--breaks programs up by $'s
splitByEOP :: String -> [String] -> [String]
splitByEOP ((fst@('$')):i) lst = splitByEOP i lst
splitByEOP [] lst = lst
splitByEOP i lst = splitByEOP (dropWhile (/='$') i) (lst++[(takeWhile (/='$') i)])


test :: IO()
test = do
    putStrLn "INPUT: {}${{{{{{}}}}}}${{{{{{}}}}}}}${int @}$"
    print (map (tokenize) (splitByEOP "{}${{{{{{}}}}}}${{{{{{}}}}}}}${int @}$" []))