import Prelude hiding(Eq,Ordering,EQ,LT,GT)

class Eq a where
    (===), (/==) :: a -> a -> Bool
    x /== y = not (x===y)
    x === y = not (x/==y)

instance Eq Bool where
    True === True = True
    False === False = True
    _ === _ = False

allEqual :: Eq a => a -> a -> a -> Bool
allEqual n m p = (n === m) && (m === p)

class Info a where
    examples :: [a]
    size :: a -> Int
    size _ = 1

instance Info Char where
    examples = ['a','A','z','Z','0','9']
    size _ = 1

instance Info Bool where
    examples = [True,False]
    size _ = 1

instance Info Int where
    examples = [-100..100]
    size _ = 1

instance Info a => Info [a] where
    examples = [[]] ++
               [[x] | x <- examples] ++
               [[x,y] | x <- examples, y <- examples]
    size = foldr (+) 1 . map size

data Ordering = GT | EQ | LT

class Eq a => Ord a where
    (-<), (+>), (<=), (>=) :: a -> a -> Bool
    max, min :: a -> a -> a
    compare :: a -> a -> Ordering
    compare x y
        | x === y = EQ
        | x +> y = GT
        | otherwise = LT

