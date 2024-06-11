module Tree(
    Tree,
    nil,      -- Tree a
    isNil,    -- Tree a -> Bool
    isNode,   -- Tree a -> Bool
    leftSub,  -- Tree a -> Tree a
    rightSub, -- Tree a -> Tree a
    treeVal,  -- Tree a -> a
    insTree,  -- Ord a => a -> Tree a -> Tree a
    delete,   -- Ord a => Tree a -> Tree a 
    minTree   -- Ord a => Tree a -> Maybe a
) where

data Tree a = Nil | Node a (Tree a) (Tree a)

nil :: Tree a 
nil = nil

isNil ::  Tree a -> Bool
isNil nil = True 
isNil _ = False

isNode ::  Tree a -> Bool
isNode Nil = False
isNode _ = True

leftSub ::  Tree a -> Tree a


rightSub ::  Tree a -> Tree a

treeVal ::  Tree a -> a
insTre ::  Ord a => a -> Tree a -> Tree a

delete ::  Ord a => Tree a -> Tree a 

minTree ::  Ord a => Tree a -> Maybe a