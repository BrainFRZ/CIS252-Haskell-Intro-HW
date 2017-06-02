-- Name:     Terry Weiss (466751950)
-- Email:    ttweiss@syr.edu
-- Section:  M013 LSB-105
-- Project:  Homework 8

import BinaryTrees


-- Exercise 1 - Calculates the height of a binary tree
height :: BTree a -> Int
height Empty = -1
height (BNode _ Empty Empty) = 0
height (BNode _ l r) = 1 + max (height l) (height r)


-- Exercise 2 - Returns a binary tree without any of its leafs
autumn :: BTree a -> BTree a
autumn Empty = Empty
autumn (BNode _ Empty Empty) = Empty
autumn (BNode v l r) = BNode v (autumn l) (autumn r)


-- Exercise 3 - Determines whether a binary tree is full
full :: BTree a -> Bool
full Empty = True
full (BNode _ Empty (BNode _ _ _)) = False
full (BNode _ (BNode _ _ _) Empty) = False
full (BNode _ l r) = full l && full r


-- Exercise 4 - Returns the mirror image of a binary tree
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (BNode v l r) = BNode v (mirror r) (mirror l)


-- Exercise 5 - Rebuilds a binary tree with a function applied to each node
mapTree :: (a -> b) -> BTree a -> BTree b
mapTree _ Empty = Empty
mapTree f (BNode v l r) = BNode (f v) (mapTree f l) (mapTree f r)


-- Exercise 6 - Builds a list of all nodes at a given depth
depthVals :: Int -> BTree a -> [a]
depthVals _ Empty = []
depthVals n (BNode v l r)
    | n < 0     = []
    | n == 0    = [v]
    | otherwise = (depthVals (n-1) l) ++ (depthVals (n-1) r)


-- Exercise 7 - Calculates a tree's smallest element, or value if Empty
minValue :: Ord a => a -> BTree a -> a
minValue val Empty = val
minValue _ tree = minimum (nodes tree)
    where nodes :: BTree a -> [a]
          nodes Empty = []
          nodes (BNode v l r) = v : (nodes l) ++ (nodes r)


-- Exercise 8 - List of nodes in the given path from the root
onPath :: Path -> BTree a -> [a]
onPath _ Empty = []
onPath [] (BNode v _ _)        = [v]
onPath (Lft:ds) (BNode v l _)  = v : onPath ds l
onPath (Rght:ds) (BNode v _ r) = v : onPath ds r
