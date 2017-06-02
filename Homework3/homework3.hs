-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 3

-- Exercise 1 - Generates a list of `n` pairs ascending from i
squarePairs :: Int -> Integer -> [(Integer,Integer)]
squarePairs n i
    | n <= 0     = []
    | otherwise  = (i, i*i) : squarePairs (n-1) (i+1)

-- Exercise 2 - Generates a list from `m` down to `n` in increments of `diff`
countDownBy :: Int -> Int -> Int -> [Int]
countDownBy m n diff
    | diff <= 0 || m < n  = []
    | otherwise           = m : countDownBy (m - diff) n diff

-- Exercise 3 - Generates a list of interval lists of incremental
--              length from `m` to `n`
steps :: Int -> Int -> [[Int]]
steps m n
    | n < m      = [[]]
    | otherwise  = list m
    where list :: Int -> [[Int]]
          list i
            | i > n      = []
            | otherwise  = countUp m i : list (i+1)

          countUp :: Int -> Int -> [Int]  -- countUp function created in class
          countUp a b
            | a > b      = []
            | otherwise  = a : countUp (a+1) b

-- Exercise 4 - Generates a string `n` characters `c` with '!' at element `i`
indexChar :: Int -> Int -> Char -> String
indexChar n i c
    | n <= 0     = ""
    | i == 1     = '!' : remainder
    | otherwise  =  c  : remainder
    where remainder = indexChar (n-1) (i-1) c

-- Exercise 5 - Generates a list of strings `n` characters `c` with
--              '!' replacing the i-th character left-to-right.
diag :: Int -> Char -> [String]
diag n c = list 1
    where list :: Int -> [String]
          list i
            | i > n      = []
            | otherwise  = indexChar n i c : list (i+1)
