import LanguageData
import Lexer
import Parser
import Grammar
import AST
import Text.Parsec (parse)
import Control.Monad (join)
import Data.Either (rights)

import Data.Tree



--RUNS the I/O show. gets called
main :: IO ()
main = do 
    introMessage
    userInput <- getLine
    parsablePrograms <- return (map (++"$") (splitByEOP userInput []))
    tokensForPrograms <- return $ appendEOPs (map (tokenize) (splitByEOP userInput []))
    putStrLn "\nLEXER: BEGIN\n" 
    lexProgramsOUT tokensForPrograms
    
    --print (parsablePrograms)
    putStrLn "\nPARSER: BEGIN\n" 
    parseProgramsOUT parsablePrograms
    --remove space tokens
    tokensForPrograms <- return$map (filter (/=T_space)) tokensForPrograms
    print tokensForPrograms
    --print (head$rights [(parse programP "Program" (head parsablePrograms))])
    treeDataForPrograms <- (getTreeData parsablePrograms [])
    --print treeDataForPrograms

    putStrLn("\nSEMANTIC ANALYSIS: REACHED!\n\n")

    putStrLn("AST-------------------------------------------------------------------\n")

    treeExample <- return (makeAST (head treeDataForPrograms) (head tokensForPrograms))
    
    putStrLn "\nRaw Tree Object:\n"
    print treeExample
    putStrLn "\nTree:\n"
    putStrLn $ drawTree treeExample
 
    
    


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



getTreeData :: [String] -> [PROGRAM] -> IO ([PROGRAM])
getTreeData [] xs = do
    return xs
getTreeData (p:ps) xs = do
    x <- (getProgram p)
    getTreeData ps (x:xs)

getProgram :: String -> IO (PROGRAM)
getProgram p = do return$head$rights [(parse programP "Program" p)]


parseProgramsOUT :: [String] -> IO ()
parseProgramsOUT [] = do
    putStrLn "PARSER: END"
parseProgramsOUT (p:ps) = do
    (parseProgramOUT p)
    (parseProgramsOUT ps)

parseProgramOUT :: String -> IO ()
parseProgramOUT p = do
    putStrLn "PARSER: Parsing Program...\n"
    putStrLn "PARSER: Right = Success, returning CST as a Linked List." 
    putStrLn "PARSER: Left  = Failure"
    putStr "PARSER: "
    print (parse programP "Program" p)
    putStr "\n"
    --print (head$rights [(parse programP "Program" p)])
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
    putStrLn ("LEXER: Lexing Program...\n")
    (lexProgramOUT p 1)
lexProgramOUT (t:ts) n = do
    putStrLn ("LEXER: token found:\t"++(makeTerminal t))
    (lexProgramOUT ts (n+1))



stripQuotesFromNode :: Tree String -> Tree String
stripQuotesFromNode n@(Node str cs) = Node (filter (/='\"') str) cs



--alternative test function ran through gchi. (lexer only)
test :: IO()
test = do
    putStrLn "INPUT: {int a a = 5 if (a==5) {print(a) int b b = 4} a = b}$"
    lexProgramsOUT (map (tokenize) (splitByEOP "{int a a = 5 if (a==5) {print(a) int b b = 4} a = b}$" []))