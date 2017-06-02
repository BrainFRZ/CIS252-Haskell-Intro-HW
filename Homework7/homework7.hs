-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 7

import Data.Char


-- Exercise 1 - Lists all indicies of a given element in a list
locate :: Eq a => a -> [a] -> [Int]
locate x ys = map fst (filter (\(n,y) -> y == x) (zip [1..] ys))


-- Exercise 2 - Generates a histogram given a list of bar lengths
histogram :: [Int] -> String
histogram xs = concatMap (\n -> (replicate n '*') ++ "\n") xs


-- Exercise 3 - Applies each function in a list to a value
manyFuns :: [a -> b] -> a -> [b]
manyFuns fs v = map (\f -> f v) fs


-- Exercise 4 - Generalized insertion sort using a given predicate
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = ins x (isort xs)
  where
    ins y []         = [y]
    ins y (z:zs)
        | y <= z     = y:z:zs
        | otherwise  = z: (ins y zs)

mySort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
mySort _ [] = []
mySort p (x:xs) = ins x (mySort p xs)
  where
    ins y []         = [y]
    ins y (z:zs)
        | y `p` z    = y:z:zs
        | otherwise  = z: (ins y zs)

-- Exercise 5 - Determines if a value is a fixed point of a function
isFixPt :: Eq a => (a -> a) -> a -> Bool
isFixPt f val = val == f val


-- Exercise 6 - Generates a new list that is identical if no element
--              matches the predicate, or else replaces the first matching
--              element with the given value
changeFirst :: (a -> Bool) -> a -> [a] -> [a]
changeFirst _ _ [] = []
changeFirst p val (x:xs)
    | p x        = val : xs
    | otherwise  = x : (changeFirst p val xs)
