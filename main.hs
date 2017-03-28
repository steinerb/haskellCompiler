import LanguageData
import Lexer
import Parser


--RUNS the I/O show. gets called
main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    tokensForPrograms <- return (map (tokenize) (splitByEOP userInput []))
    lexPrograms tokensForPrograms

    --putStrLn (parseProgram (head (tokensForPrograms)))



--NEED TO MAKE IO FOR PARSER!!!!!


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


lexPrograms :: [[Token]] -> IO ()
lexPrograms [] = putStrLn "LEXER: END"
lexPrograms (p:ps) = do
    (lexProgram p 0)
    (lexPrograms ps)


lexProgram :: [Token] -> Int -> IO ()
lexProgram [] _ = do 
    putStrLn "\nLEXER: Program Completed Successfully!\n"
lexProgram p 0 = do
    putStrLn ("Lexing Program...\n")
    (lexProgram p 1)
lexProgram (t:ts) n = do
    putStrLn ("LEXER: token found:\t"++(makeTerminal t))
    (lexProgram ts (n+1))



--alternative test function ran through gchi.
test :: IO()
test = do
    putStrLn "INPUT: {}${{{{{{}}}}}}${{{{{{}}}}}}}${int @}$"
    lexPrograms (map (tokenize) (splitByEOP "{}${{{{{{}}}}}}${{{{{{}}}}}}}${int @}$" []))