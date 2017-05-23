import LanguageData
import Lexer
import Parser


--RUNS the I/O show. gets called
main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    tokensForPrograms <- return $ appendEOPs (map (tokenize) (splitByEOP userInput []))
    lexProgramsOUT tokensForPrograms

    --line for parsing programs here!

    --parseProgramOUT (head tokensForPrograms)
    
    

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

appendEOPs :: [[Token]] -> [[Token]]
appendEOPs pgmTokens = map (++[T_EOP]) pgmTokens

--PARSER OUTPUT
parseProgramsOUT :: [[Token]] -> IO ()
parseProgramsOUT [] = putStrLn "PARSER: END"
parseProgramsOUT (p:ps) = do
    (parseProgramOUT p)
    (parseProgramsOUT ps)



parseProgramOUT :: [Token] -> IO ()
parseProgramOUT p = putStrLn "PARSER: single program parse reached!"



lexProgramsOUT :: [[Token]] -> IO ()
lexProgramsOUT [] = putStrLn "LEXER: END"
lexProgramsOUT (p:ps) = do
    (lexProgramOUT p 0)
    (lexProgramsOUT ps)


lexProgramOUT :: [Token] -> Int -> IO ()
lexProgramOUT [] _ = do 
    putStrLn "\nLEXER: Program Completed Successfully!\n"
lexProgramOUT p 0 = do
    putStrLn ("Lexing Program...\n")
    (lexProgramOUT p 1)
lexProgramOUT (t:ts) n = do
    putStrLn ("LEXER: token found:\t"++(makeTerminal t))
    (lexProgramOUT ts (n+1))



--alternative test function ran through gchi.
test :: IO()
test = do
    putStrLn "INPUT: {}${{{{{{}}}}}}${{{{{{}}}}}}}${int @}$"
    lexProgramsOUT (map (tokenize) (splitByEOP "{}${{{{{{}}}}}}${{{{{{}}}}}}}${int @}$" []))