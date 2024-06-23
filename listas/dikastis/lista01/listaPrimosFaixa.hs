-- Implemente a função:

-- primos :: [Int] -> [Int]
-- Dada uma lista de inteiros, essa função retorna uma nova lista, apenas com os elementos da lista original que 
-- não são divisores exatos de qualquer dos elementos posteriores a eles.

-- A função primos pode ser recursiva. A função primos aplicada a uma lista unitária, devolve a mesma lista 
-- unitária.

-- Além disso, defina uma função:

-- primosN :: Int -> [Int]
-- Que retorna o resultado da primeira função, primos, para uma lista de 1 a n.

naoDivisor :: Int -> [Int] -> Bool
naoDivisor _ [] = True
naoDivisor x (y:ys)
    | y `mod` x == 0 = False
    | otherwise = naoDivisor x ys 

primos :: [Int] -> [Int]
primos [] = []
primos (x:xs)
    | naoDivisor x xs = x: primos xs 
    | otherwise = primos xs

primosN :: Int -> [Int]
primosN n = primos [1..n]



main = interact $ show . primosN . (read :: String -> Int)