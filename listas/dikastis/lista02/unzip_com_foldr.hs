import Prelude hiding (unzip)

-- Considere unzip:: [(a,b)] -> ([a],[b]), definida como:

unzip:: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((a, b): xs) = (a:as, b:bs)
 where
   (as, bs) = unzip xs 
-- Exemplo:

-- unzip [(1,2), (3,4), (5,6)] ==> ([1,3,5],[2,4,6])
-- Defina a função unzip', usando foldr.

-- Use a seguinte main:

-- vamos testar apenas com inteiros
-- main = interact $ show . unzip' . (read :: String -> [(Int,Int)])
-- Para isso, defina unzip' com o tipo adequado.

unzip' :: [(a,b)] -> ([a],[b])
unzip' = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([], [])

main = interact $ show . unzip' . (read :: String -> [(Int,Int)])