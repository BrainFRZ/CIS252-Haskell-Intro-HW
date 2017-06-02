-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 4

import Data.Char

-- Exercise 1 - Calculates the product of a list of numbers
myProduct :: [Integer] -> Integer
myProduct []     = 1
myProduct (0:_)  = 0  -- always return 0 if multiplying by 0
myProduct (n:ns) = n * (myProduct ns)


-- Exercise 2 - Converts all lower-case letters in a string to upper-case
shout :: String -> String
shout "" = ""
shout (c:cs)
    | isLower c  = toUpper c : shout cs
    | otherwise  = c : shout cs


-- Exercise 3 - Removes all instances of a character from a string
zap :: Char -> String -> String
zap _ "" = ""
zap ch (c:cs)
    | ch == c    = zap ch cs
    | otherwise  = c : zap ch cs


-- Exercise 4 - Generates a list by pairing elements in a list. If the list
--              has an odd length, the last element is duplicated.
pairUp :: [a] -> [(a,a)]
pairUp (x:y:zs) = (x,y) : pairUp zs
pairUp (z:zs)   = [(z,z)]
pairUp []       = []


-- Exercise 5 - Generates a list of all pairs of "neighboring" elements
neighbors :: [a] -> [(a,a)]
neighbors (x:y:zs) = (x,y) : neighbors (y : zs)
neighbors _        = []


-- Exercise 6a - Calculates the total number of items in bag
bagCount :: [(Char,Int)] -> Int
bagCount []         = 0
bagCount ((c,i):es) = i + bagCount es

-- Exercise 6b - Adds a character to bag and returns the new bag
addToBag :: Char -> [(Char,Int)] -> [(Char,Int)]
addToBag ch []  = [(ch,1)]
addToBag ch ((c,i):es)
    | ch == c    = (c,i+1) : es
    | otherwise  = (c,i)   : addToBag ch es

-- Exercise 6c - Removes a character from bag
removeFromBag :: Char -> [(Char,Int)] -> [(Char,Int)]
removeFromBag _ [] = []
removeFromBag ch ((c,i):es)
    | ch /= c    = (c,i)   : removeFromBag ch es
    | i > 1      = (c,i-1) : es
    | otherwise  = es
