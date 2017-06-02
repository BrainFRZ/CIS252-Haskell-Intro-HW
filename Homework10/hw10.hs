-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 10

getInteger :: IO Integer
getInteger = do line <- getLine
                return (read line::Integer)

getFloat :: IO Float
getFloat = do line <- getLine
              return (read line::Float)


-- Exercise 1 - Prompts 3 Floats and returns a pair of their mean and median
stats :: IO (Float, Float)
stats = do putStrLn "Please enter three numbers:"
           ns <- sequence [getFloat | _ <- [1..3]]
--         return ((sum ns) / 3, minimum [n | n <- ns, minimum ns /= n])  --doesn't work if x=y=z
           return ((sum ns) / 3, sum ns - minimum ns - maximum ns)


-- Exercise 2 - Displays a string vertically and returns its length
printVert :: String -> IO Int
printVert "" = do putStrLn ""
                  return 0
printVert cs = do sequence [putStrLn [c] | c <- cs]
                  return (length cs)


-- Exercise 3 - Prompts a line of text and displays each word on a line
displayWords :: IO ()
displayWords = do putStr "Please enter a line of text: "
                  ws <- getLine
                  sequence_ [putStrLn w | w <- words ws]


-- Exercise 4 - Prompts a line of text and displays each word on a numbered line
displayWords2 :: IO ()
displayWords2 = do putStr "Please enter a line of text: "
                   ws <- getLine
                   sequence_ [putStrLn ((show l) ++ ".  " ++ w)
                                    | (l,w) <- zip [1..] (words ws)]


-- Exercise 5 - Reads a series of integers until 0 is entered
nonzeros :: IO [Integer]
nonzeros = do n <- getInteger
              if n == 0
                then return []
                else do ns <- nonzeros
                        return (n : ns)

-- Exercise 6 - Reads a series of integers until 0 is entered, then displays the
--              number of nonzero integers, number of positive integers, and the
--              smallest negative integer.
posAndNegs :: IO ()
posAndNegs = do putStrLn "Please enter a series of integers (0 to terminate):"
                ns <- nonzeros
                putStrLn ("Number of nonzero values entered: "
                            ++ (show (length ns)))
                putStrLn ("Number of positive entered: "
                            ++ (show (length (filter (>0) ns))))
                if null ns || minimum ns > 0
                    then putStrLn ("You did not enter any negative numbers.")
                    else putStrLn ("Smallest negative number: "
                                        ++ show (minimum ns))
