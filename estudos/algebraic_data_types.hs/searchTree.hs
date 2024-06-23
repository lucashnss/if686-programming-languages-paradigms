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

isNode ::  Tree a -> Bool
isNode Nil = False
isNode _ = True

leftSub ::  Tree a -> Tree a
leftSub Nil = error "Nó vazio"
leftSub (Node _ tl _) = tl

rightSub ::  Tree a -> Tree a
rightSub Nil = error "Nó vazio"
rightSub (Node _ _ tr) = tr 

treeVal ::  Tree a -> a
treeVal Nil = error "Nó vazio"
treeVal (Node value _ _) = value 

insTree ::  Ord a => a -> Tree a -> Tree a
insTree val Nil = Node val Nil Nil 
insTree val (Node v tl tr)
    | v == val = Node v tl tr 
    | val > v = Node v tl (insTree val tr)
    | val < v = Node v (insTree val tl) tr 

delete ::  Ord a => a -> Tree a -> Tree a 
delete _ Nil = Nil
delete val (Node v t1 t2)
    | val < v = Node v (delete val t1) t2
    | val > v = Node v t1 (delete val t2)
    | isNil t2 = t1
    | isNil t1 = t2
    | otherwise = join t1 t2

minTree :: Ord a => Tree a -> Maybe a
minTree t
    | isNil t = Nothing
    | isNil t1 = Just v
    | otherwise = minTree t1
    where
        t1 = leftSub t
        v = treeVal t

join :: Ord a => Tree a -> Tree a -> Tree a
join t1 t2 = Node mini t1 newt
    where
        (Just mini) = minTree t2
        newt = delete mini t2
