import LanguageData
import Lexer
import Parser


--RUNS the I/O show. gets called
main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    tokensForPrograms <- return (map (tokenize) (splitByEOP userInput []))
    printPrograms tokensForPrograms

    --tokensForPrograms <- parseTokens tokensForPrograms
    --NEED TO PARSE tokensForPrograms!!


--first printed text
introMessage :: IO ()
introMessage = do putStrLn ("Enter a String to be tokenized!\nWorking Tokens Include: \n\t"++workingTokens)

workingTokens :: String
workingTokens = "{, }, (, ), +, =, ==, !=, true, false, while, if, print, int, string, boolean, single character variables, or a string literal: [\"...\"]"

--breaks programs up by $'s
splitByEOP :: String -> [String] -> [String]
splitByEOP ((fst@('$')):i) lst = splitByEOP i lst
splitByEOP [] lst = lst
splitByEOP i lst = splitByEOP (dropWhile (/='$') i) (lst++[(takeWhile (/='$') i)])


printPrograms :: [[Token]] -> IO ()
printPrograms [] = putStrLn "LEXER: END"
printPrograms (p:ps) = do
    (printProgram p 0)
    (printPrograms ps)


printProgram :: [Token] -> Int -> IO ()
printProgram [] _ = do 
    putStrLn "\nLEXER: Program Completed Successfully!\n"
printProgram p 0 = do
    putStrLn ("Lexing Program...\n")
    (printProgram p 1)
printProgram (t:ts) n = do
    putStrLn ("LEXER: token found:\t"++(makeTerminal t))
    (printProgram ts (n+1))



--alternative test function ran through gchi.
test :: IO()
test = do
    putStrLn "INPUT: {}${{{{{{}}}}}}${{{{{{}}}}}}}${int @}$"
    printPrograms (map (tokenize) (splitByEOP "{}${{{{{{}}}}}}${{{{{{}}}}}}}${int @}$" []))