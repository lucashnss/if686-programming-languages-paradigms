-- Defina a função:

-- maxFour :: Integer −> Integer −> Integer −> Integer −> Integer 
-- Que retorna o máximo de quatro inteiros. Dê três definições dessa função: a primeira (com assinatura maxFour) 
-- descrita com base em maxThree; A segunda (com assinatura maxFour') deve usar a função max e a terceira 
-- (com assinatura maxFour'') deve usar as funções max e maxThree.


maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z 
  | x >= y && x >= z = x 
  | y >= z = y 
  | otherwise = z

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d 
  | maxThree a b c <= d = d 
  | otherwise = maxThree a b c

maxFour' :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour' a b c d = max a (max b (max c d))

maxFour'' :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour'' a b c d = max a (maxThree b c d)

parseInput :: Read a => String -> [a]
parseInput = map read . words
bigUncurry f [a, b, c, x] = f a b c x

maxFours m n p q = (maxFour m n p q, maxFour' m n p q, maxFour'' m n p q)

main :: IO()
main = interact $ show . bigUncurry maxFours . parseInput