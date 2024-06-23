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

type Codigo = Integer
data Voto = Presidente Codigo | Senador Codigo | Deputado Codigo | Branco 
            deriving (Show)
type Urna = [Voto]
type Apuracao = [(Voto,Integer)]

instance Eq Voto where
    Branco == Branco = True 
    Presidente p1 == Presidente p2 = p1 == p2
    Senador s1 == Senador s2 = s1 == s2
    Deputado d1 == Deputado d2 = d1 == d2
    _ == _ = False

umaUrna :: [Voto]
umaUrna= [(Presidente 1), (Presidente 1) , 
 (Senador 1),(Senador 1),(Senador 1),
 (Presidente 2), (Presidente 3),
 (Deputado 1), (Deputado 1),(Deputado 1),
 (Deputado 1), (Deputado 1),(Deputado 1), 
 (Deputado 1), (Deputado 1),(Deputado 1),
 Branco, Branco, (Presidente 1)
 ]

umaApuracao :: [(Voto, Int)]
umaApuracao = [((Presidente 1), 1), ((Senador 1),3)]

totalVotos :: Urna -> Voto -> Int 
totalVotos [] _ = 0
totalVotos (x:xs) v 
    | x == v = 1 + totalVotos xs v 
    | otherwise = totalVotos xs v

apurar :: Urna -> Apuracao -> Apuracao
apurar (v:[]) ap = atualizarApuracaoVoto v ap
apurar (v:vs) ap = apurar vs (atualizarApuracaoVoto v ap)     

apurar2 [] = []
apurar2 (x:xs) = (x, 1 + totalVotos xs x): apurar2 (filter (/=x) xs) 

atualizarApuracaoVoto voto ap
 | votoRegistrado ap voto = atualizarApuracao ap voto
 | otherwise = ap ++ [(voto,1)]


atualizarApuracao :: [(Voto, Integer)] -> Voto -> [(Voto, Integer)]
atualizarApuracao ([]) voto = []
atualizarApuracao (x:xs) voto
  | fst(x) == voto = ((voto, (snd x) + 1)):xs
  | otherwise = x: atualizarApuracao xs voto

  
votoRegistrado :: [(Voto, Integer)] -> Voto -> Bool
votoRegistrado [] voto = False
votoRegistrado (x:xs) voto = fst(x) == voto || votoRegistrado xs voto

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