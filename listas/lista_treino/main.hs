-- map . (.) = ((t -> u) -> [t] -> [u]) . ((b -> c) -> (a -> b) -> a -> c)
--            |----------|   |--------|     |----|   |------------------|
--                 b            c             a               b
--            -------------
--            (a -> b) -> (a -> c)
--               t         u   
-- map . (.) :: (b -> c) -> ([a -> b]-> [a -> c])

-- Questao 2 
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = [x:ys | ys <- sublistas xs] ++ sublistas xs

-- Questao 3 
poli :: Integer -> Integer -> Integer -> Integer -> Integer 
poli a b c = \x -> a*x*x + b*x + c

listaPoli :: [(Integer,Integer,Integer)] -> [Integer->Integer]
listaPoli l = [poli x y z | (x,y,z) <- l]

appListaPoli :: [Integer->Integer] -> [Integer] -> [Integer]
appListaPoli [] [] = []
appListaPoli (x:xs) (y:ys) = x y: appListaPoli xs ys
appListaPoli lPoli lInt = [f x | (f,x) <- zip lPoli lInt]

-- Questao 4
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
    take (y-x-1) (drop x m) ++
    [m !! (x-1)] ++
    drop y m

extraiPrincipal :: [[Integer]] -> Int -> [Integer]
extraiPrincipal l 0 = [(l!!0)!!0]
extraiPrincipal l i = (l!!i)!!i : extraiPrincipal l (i-1)

-- diagPrincipal :: [[Integer]] -> [Integer]
diagPrincipal :: [[Integer]] -> [Integer]
diagPrincipal l = reverse (extraiPrincipal l((length l)-1))

-- Questao 5 






-- Questao 6
data Pilha t = Pilha [t] | PilhaVazia
            deriving(Show)

push :: t -> Pilha t -> Pilha t
push x PilhaVazia = Pilha [x]
push x (Pilha t) = Pilha (x:t)

pop :: Pilha t -> Pilha t 
pop PilhaVazia  = error "Pilha Vazia"
pop (Pilha (x:xs)) = Pilha xs  

top :: Pilha t -> t 
top PilhaVazia = error "Pilha Vazia"
top (Pilha (x:xs)) = x 