module BinaryTrees where

----------------------------------------------------------------------
--
--   The following definition does the following:
--
--       (1) Creates a family of datatypes of the form BTree a
--              Examples:  BTree Int, BTree Bool, BTree [Float], etc.
--
--       (2) Creates the (polymorphic!) constructor value:
--               Empty :: BTree a
--
--       (3) Creates the (polymorphic!) constructor function:
--               BNode :: a -> BTree a -> BTree a -> BTree a
--
--           Example:
--                (BNode 13 Empty Empty) :: BTree Int
--                (BNode 'M' Empty Empty) :: BTree Char
--
--       (4) Adds each type  of form  BTree a to the Show class
----------------------------------------------------------------------

data BTree a = Empty 
             | BNode a (BTree a) (BTree a)
               deriving (Show)


-- a datatype for directions
data Dir = Lft | Rght
type Path = [Dir]


----------------------------------------------------------------------
-- Some sample trees
----------------------------------------------------------------------

--
-- tree1:         7
--

tree1 :: BTree Int
tree1 = BNode 7 Empty Empty

--
-- tree2:         8
--               / \
--              13  20
--                \
--                 12
--

tree2 :: BTree Int
tree2 = BNode 8 (BNode 13 Empty 
                          (BNode 12 Empty Empty)) 
                (BNode 20 Empty Empty)



--
-- tree3:         100
--                / \
--               75  120
--              /  \   \
--            65    82  147
--                 /    / \
--                78  134  153
--

tree3 :: BTree Int
tree3 = BNode 100  (BNode 75 (BNode 65 Empty Empty)
                             (BNode 82 (BNode 78 Empty Empty) Empty))
                   (BNode 120 Empty
                              (BNode 147 (BNode 134 Empty Empty)
                                         (BNode 153 Empty Empty)))


--
-- tree4:      "here's"
--               / \
--             "a" "string"
--                \
--                 "tree"
--

tree4 :: BTree String
tree4 = BNode "here's" (BNode "a" Empty 
                                 (BNode "tree" Empty Empty)) 
                       (BNode "string" Empty Empty)


--
--
-- tree5:         100
--                / \
--              75   120
--             /  \   
--           65    82 
--                /  \
--               78  134  
--
--    A full tree, but not perfect

tree5 :: BTree Int
tree5 = BNode 100 (BNode 75 (BNode 65 Empty Empty)
                            (BNode 82 (BNode 78 Empty Empty)
                                      (BNode 134 Empty Empty)))
                  (BNode 120 Empty Empty)
