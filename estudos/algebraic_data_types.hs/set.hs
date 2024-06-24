
module Set ( 
    Set,    
    empty,              --  Set a 
    sing,               --  a -> Set a 
    memSet,             -- Ord a => a -> Set a -> Bool
    union,inter,  -- Ord a => Set a -> Set a -> Set a 
    eqSet,              -- Eq a => Set a -> Set a -> Bool
    subSet,             -- Ord a => Set a -> Set a -> Bool
    makeSet,            -- Ord a => [a] -> Set a
    mapSet,             -- Ord b => (a -> b) -> Set a -> Set b 
    filterSet,          -- (a -> Bool) -> Set a -> Set a
    foldSet,            -- (a -> a -> a) -> a -> Set a -> a
    showSet,            -- (a -> String) -> Set a -> String
    card                -- Set a -> Int
) where

import Data.List hiding (union)

newtype Set a = Set [a]

instance Eq a => Eq (Set a) where 
    (==) = eqSet

instance Ord a => Ord (Set a) where 
    (<=) = leqSet 

empty :: Set a 
empty = Set []

sing :: a -> Set a 
sing x = Set [x]

memSet :: Ord a => Set a -> a -> Bool
memSet (Set []) y = False 
memSet (Set (x:xs)) y
    | x < y = memSet (Set xs) y 
    | x == y = True 
    | otherwise = False 

union :: Ord a => Set a -> Set a -> Set a 
union (Set xs) (Set ys) = Set (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni xs [] = xs 
uni [] ys = ys 
uni (x:xs) (y:ys)
    | x < y = x: uni xs (y:ys)
    | x == y = x: uni xs ys 
    | otherwise = y: uni (x:xs) ys

inter :: Ord a => Set a -> Set a -> Set a 
inter (Set xs) (Set ys) = Set (int xs ys)

int :: Ord a => [a] -> [a] -> [a]
int [] ys = ys 
int xs [] = xs 
int (x:xs) (y:ys)
    | x < y = int xs (y:ys)
    | x == y = x: int xs (y:ys)
    | otherwise = int (x:xs) y 

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set xs) (Set ys) = subS xs ys 

subS :: Ord a => [a] -> [a] -> Bool 
subS [] ys = True
subS xs [] = False 
subs (x:xs) (y:ys)
    | x < y = False 
    | x == y = subS xs ys 
    | x > y = subS (x:xs) ys

eqSet :: Eq a => Set a -> Set a -> Bool 
eqSet (Set xs) (Set ys) = xs == ys 

leqSet :: Ord a => Set a -> Set a -> Bool 
leqSet (Set xs) (Set ys) = xs <= ys 

makeSet :: Ord a => [a] -> Set a 
makeSet = Set . remDumps . sort 
    where 
        remDumps [] = []
        remDumps [x] = [x]
        remDumps (x:y:xs)
            | x < y = x: remDumps (y:xs)
            | otherwise = remDumps (y:xs)

mapSet :: Ord b => (a -> b) -> Set a -> Set b 
mapSet f (Set xs) = makeSet (map f xs)

filterSet :: (a -> Bool) -> Set a -> Set a 
filterSet p (Set xs) = Set (filter p xs)

foldSet :: (a -> a -> a) -> a -> Set a -> a 
foldSet f x (Set xs) = foldr f x xs


showSet :: (a -> String) -> Set a -> String 
showSet f (Set xs) = concatMap ((++"\n").f) xs

card :: Set a -> Int 
card (Set xs) = length xs 

-- Relations

type Relation a = Set (a,a)

image :: Ord a => Relation a -> a -> Set a 
image rel val = mapSet snd (filterSet ((==val).fst) val)

setImage :: Ord a => Relation a -> Set a -> Set a 
setImage rel = unionSet . mapSet (image rel)


unionSet :: Ord a => Set (Set a) -> Set a 
unionSet = foldSet union empty

addImage :: Ord a => Relation a -> Set a -> Set a 
addImage rel st = st `union` setImage rel st 

compose :: Ord a => Relation a -> Relation a -> Relation a 
compose rel1 rel2 
    = mapSet outer (filterSet equals (setProduct rel1 rel2))
    where 
        equals ((a,b),(c,d)) = b==c
        outer  ((a,b),(c,d)) = (a,d)

setProduct :: (Ord a,Ord b) => Set a -> Set b -> Set (a,b)
setProduct st1 st2 = unionSet (mapSet (adjoin st1) st2)

adjoin :: (Ord a,Ord b) => Set a -> b -> Set (a,b)
adjoin st el = mapSet (addEl el) st 
               where 
                    addEl el el' = (el',el)

tClosure :: Ord a => Relation a -> Relation a 
tClosure rel = limit addGen rel 
               where 
                    addGen rel' = rel' `union` (rel' `compose` rel)

limit :: Eq a => (a -> a) -> a -> a
limit f x 
    | x == next = x 
    | otherwise = limit f next 
        where 
            next = f x 