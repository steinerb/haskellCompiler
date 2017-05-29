import LanguageData
import Lexer
import Parser
import Grammar
import Text.Parsec (parse)
import Control.Monad (join)
import Data.Either (rights)

--RUNS the I/O show. gets called
main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    parsablePrograms <- return (map (++"$") (splitByEOP userInput []))
    tokensForPrograms <- return $ appendEOPs (map (tokenize) (splitByEOP userInput []))
    lexProgramsOUT tokensForPrograms
    --print tokensForPrograms
    print (parsablePrograms)
    putStrLn "\nPARSER: BEGIN\n" 
    parseProgramsOUT parsablePrograms

    putStrLn("\nSEMANTIC ANALYSIS: REACHED!")

    --parseProgramOUT (head tokensForPrograms)
    
    


--first printed text
introMessage :: IO ()
introMessage = do putStrLn ("Enter a String to be tokenized!\nWorking Tokens Include: \n\t"++workingTokens)

workingTokens :: String
workingTokens = "{, }, (, ), +, =, ==, !=, true, false, while, if, print, int, string, boolean, single character variables, or a string literal: \"...\""

--breaks programs up by $'s
splitByEOP :: String -> [String] -> [String]
splitByEOP ((fst@('$')):i) lst = splitByEOP i lst
splitByEOP [] lst = lst
splitByEOP i lst = splitByEOP (dropWhile (/='$') i) (lst++[(takeWhile (/='$') i)])

appendEOPs :: [[Token]] -> [[Token]]
appendEOPs pgmTokens = map (++[T_EOP]) pgmTokens

--PARSER OUTPUT
--parseProgramsOUT :: [String] -> [IO (PROGRAM)] -> IO ([IO (PROGRAM)])
--parseProgramsOUT [] xs = do
--    putStrLn "PARSER: END"
--    return xs
--parseProgramsOUT (p:ps) xs = do
--    x <- return (parseProgramOUT p)
--    (parseProgramsOUT ps (x:xs))


parseProgramsOUT :: [String] -> IO ()
parseProgramsOUT [] = do
    putStrLn "PARSER: END"
parseProgramsOUT (p:ps) = do
    (parseProgramOUT p)
    (parseProgramsOUT ps)

parseProgramOUT :: String -> IO ()
parseProgramOUT p = do
    putStrLn "PARSER: Parsing Program...\n"
    putStrLn "PARSER: Right = Success, Left = Failure"
    putStr "PARSER: "
    print (parse programP "Program" p)
    putStr "\n"
    print (head$rights [(parse programP "Program" p)])
    --return$head$rights [(parse programP "Program" p)]
--COMMENT OUT LAST LINE^^^ FOR IO () VERSION!!!


--LEXER OUTPUT
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