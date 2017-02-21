import Lexer
import Grammar

--RUNS the I/O show. gets called
main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    programs <- return (map (tokenize) (splitByEOP userInput []))
    --printPrograms programs
    printPrograms programs
--first printed text
introMessage :: IO ()
introMessage = do putStrLn ("Enter a String to be tokenized!\nWorking Tokens Include: "++workingTokens)

workingTokens :: String
workingTokens = "{, }, (, ), +, ==, !=, while, if, print, int, string, boolean"

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