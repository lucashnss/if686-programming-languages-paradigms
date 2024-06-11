import Prelude hiding (map,doubleArr,filter)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

double :: Int -> Int
double n = 2*n

doubleArr :: [Int] -> [Int]
doubleArr xs = map double xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

foldr1_ :: (a -> a -> a) -> [a] -> a
foldr1_ f [x] = x
foldr1_ f (x:xs) = f x (foldr1_ f xs) 
 
-- mais geral
foldr_ :: (a -> a -> a) -> a -> [a] -> a 
foldr_ f s [] = s 
foldr_ f s [x] = x 
foldr_ f s (x:xs) = f x (foldr_ f s xs)

-- f.g -> 
  -- f :: b -> c (g) :: a -> b (f.g) a->

addNum :: Int -> Int -> Int
addNum n m = n + m

twice :: (a -> a) -> a -> a
twice f = f.f

iter :: Int -> (a -> a) -> a -> a
iter n f 
  | n >= 0 = f . iter (n-1) f
  | otherwise = id