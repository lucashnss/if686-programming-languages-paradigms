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

maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 a b c d = max a (max b (max c d))

maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour3 a b c d = max a (maxThree b c d)

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

potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia n k = n * potencia n (k-1)


binomialCoefficients :: Integer -> Integer -> Integer
binomialCoefficients n 0 = 1
binomialCoefficients 0 k = 1
binomialCoefficients n k = binomialCoefficients (n-1) k + binomialCoefficients (n-1) (k-1)

tribonacci :: Integer -> Integer
tribonacci n 
    | n == 1  = 1
    | n == 2 = 1
    | n == 3 = 2
    | n > 3 = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

tribStep :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
tribStep (u,v,w) = (v,w,u+v+w)

tribTriple :: Integer -> (Integer,Integer,Integer)
tribTriple n
     | n == 1 = (1,1,2)
     | n == 2 = (1,2,4)
     | n > 2 = tribStep (tribTriple(n-1))

fastTribonacci :: Integer -> Integer 
fastTribonacci n 
    | n == 1 = 1
    | n == 2 = 1
    | n == 3 = 2
    | n > 2 = let (_,_,result) = tribTriple (n-2) in result

addEspacos :: Int -> String
addEspacos n 
    | n == 0 = ""
    | n > 0 = " " ++ addEspacos (n - 1)

paraDireita :: Int -> String -> String
paraDireita n str = addEspacos n ++ str

cabecalho :: String
cabecalho = "Semana    Vendas"

vendas :: Integer -> Double
vendas n 
    | n == 0 = 12
    | n == 1 = 14
    | n == 2 = 15
    | otherwise = 0

totalVendas :: Integer -> Double
totalVendas n 
    | n ==  0 = vendas 0
    | n > 0 = vendas n + totalVendas (n-1)
    | otherwise = 0

media :: Integer -> Double
media n 
    | n == 0 = totalVendas 0 
    | n > 0 = totalVendas n / fromIntegral (n + 1)