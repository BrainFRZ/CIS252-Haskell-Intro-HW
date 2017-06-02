-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 5


-- Exercise 1 - Determines whether any characters appear more than once
--              in a given string
duplicates :: String -> Bool
duplicates ""     = False
duplicates (c:cs) = c `elem` cs || duplicates cs


-- Exercise 2 - Generates a new version of a string with all instances of
--              a character removed
zap :: Char -> String -> String
zap ch cs = [c | c <- cs, c /= ch]


-- Exercise 3 - Returns a list of all characters in a string that appear exactly once
unique :: String -> String
unique "" = ""
unique (c:cs)
    | (zap c cs) == cs  = c : unique cs
    | otherwise         = unique (zap c cs)


-- Exercise 4a - Determines whether one string is the prefix of another
prefix :: String -> String -> Bool
prefix xs ys = xs == [y | (y,i) <- zip ys [1..], i <= length xs]


-- Exercise 4b - Determines whether one string is a subsequence of another
subseq :: String -> String -> Bool
subseq "" _ = True
subseq _ "" = False
subseq (x:xs) (y:ys)
    | x == y     = subseq xs ys
    | otherwise  = subseq (x:xs) ys


-- Exercise 4c - Determines whether one string is a substring of another
substring :: String -> String -> Bool
substring "" _  = True
substring _ ""  = False
substring xs b@(y:ys)
    | prefix xs b  = True
    | otherwise    = substring xs ys


-- Exercise 4d - Generates a list of all subsequences of a string
subsequences :: String -> [String]
subsequences "" = [""]
subsequences (x:xs) =  [x : w | w <- subsequences xs] ++ subsequences xs
