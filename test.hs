
main :: IO ()
main = do 
	test
	myVar <- getLine
	putStrLn myVar

test :: IO ()
test = do putStrLn "Get from line:\n"