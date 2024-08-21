import GHC.Base (TrName(TrNameD))
expNE :: Float -> Int -> Float 
expNE n 0 = 1
expNE n k = n * expNE n (k-1)

maxTwo :: Integer -> Integer -> Integer 
maxTwo x y 
    | x >= y = x 
    | otherwise = y 

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = maxTwo (maxTwo x y) z

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d = maxTwo (maxThree a b c) d

mdc :: Int -> Int -> Int 
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

numDiv :: Integral a => a -> a -> a 
numDiv a b 
    | a `mod` b == 0 = 1 + numDiv (a `div` b) b
    | otherwise = 0

merge :: Ord a => [a] -> [a] -> [a]
merge [] (y:ys) = y:ys
merge (x:xs) [] = x:xs
merge (x:xs) (y:ys)
    | x <= y = x: merge xs (y:ys)
    | otherwise = y: merge (x:xs) ys


ehDiv :: Int -> [Int] -> Bool
ehDiv _ [] = False
ehDiv x (y:ys)
    | y `mod` x == 0 = True
    | otherwise = ehDiv x ys

primos :: [Int] -> [Int]
primos [] = []
primos (x:xs)
    | ehDiv x xs = primos xs 
    | otherwise = x: primos xs

primosN :: Int -> [Int]
primosN n = primos [1..n]

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x: nub (filter (/= x) xs )

fatores :: Integer -> [Integer]
fatores n = nub (fatoresAux n 2)

fatoresAux :: Integer -> Integer -> [Integer]
fatoresAux 1 _ = []
fatoresAux n f 
    | n `mod` f == 0 = f: fatoresAux (n `div` f) f
    | otherwise = fatoresAux n (f+1)

ehPerfeito :: Integer -> Bool
ehPerfeito n = n == sum (map (^2) (fatores n))

perfeitos :: Integer -> [Integer]
perfeitos n = reverse (filter ehPerfeito [2..n]) ++ [1]