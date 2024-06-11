import Prelude hiding(max, maxThree)

dobro :: Integer -> Integer
dobro n = n * 2 

quadruplo :: (Integer -> Integer) -> Integer -> Integer
quadruplo f n =  f (f n )

poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = a * x2 + b * x + c 
    where 
        x2 = x * x

parImpar :: Integer -> String
parImpar n 
    | n `mod` 2 == 0 = "par" 
    | otherwise =  "impar"

max :: Integer -> Integer -> Integer
max x y
    | x >= y = x 
    | otherwise = y 

maxThree :: Integer -> Integer -> Integer -> Integer 
maxThree x y z = max (max x y) z

maxFour1 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour1 a b c d 
    | (a >= b) && (a >= c) && (c >= d) = a 
    | (b >= c) && (b >= d) = b 
    | c >= d = c 
    | otherwise = d

-- consertar   
maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 a b c d = a
    
quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais x y z 
    | (x == y && x == z) || (y == x && y == z) || (z==x && z==y) = 3
    | (x == y) || (y == z) || (x == z) = 2
    | otherwise = 0

ehZero :: Integer -> Bool
ehZero 0 = True
ehZero _ = False

sumTo :: Integer -> Integer
sumTo n 
    | n == 0 = 0
    | n > 0 = n + sumTo (n-1)

-- potencia :: Integer -> Integer -> Integer
