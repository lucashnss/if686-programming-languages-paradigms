-- map . (.) = ((t -> u) -> [t] -> [u]) . ((b -> c) -> (a -> b) -> a -> c)
--            |----------|   |--------|     |----|   |------------------|
--                 b            c             a               b
--            -------------
--            (a -> b) -> (a -> c)
--               t         u   
-- map . (.) :: (b -> c) -> ([a -> b]-> [a -> c])

sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = [x:ys | ys <- sublistas xs] ++ sublistas xs

poli :: Integer -> Integer -> Integer -> Integer -> Integer 
poli a b c = \x -> a*x*x + b*x + c

listaPoli :: [(Integer,Integer,Integer)] -> [Integer->Integer]
listaPoli l = [poli x y z | (x,y,z) <- l]

appListaPoli :: [Integer->Integer] -> [Integer] -> [Integer]
appListaPoli [] [] = []
appListaPoli (x:xs) (y:ys) = x y: appListaPoli xs ys
appListaPoli lPoli lInt = [f x | (f,x) <- zip lPoli lInt]

matriz :: [[Integer]]
matriz = [[1,2,3,4,5,6],[7,8,9,10,11]]

matrizQuadrada :: [[Integer]]
matrizQuadrada = [[1,2,3,4],[4,5,6,7],[8,9,10,11],[12,13,14,15]]

matrizValida :: [[Integer]] -> Bool
matrizValida m = listaValoresIguais (map length m)

listaValoresIguais :: [Int] -> Bool
listaValoresIguais (x:[]) = True 
listaValoresIguais (x:y:[]) = x == y
listaValoresIguais (x:y:xs) = y == x && listaValoresIguais xs

permutaValores :: Int -> Int -> [a] -> [a]
permutaValores x y m =
    init (take x m) ++ 
    [m !! (y-1)] ++
    (take (y-x-1) (drop x m)) ++
    [m !! (x-1)] ++
    (drop y m)

