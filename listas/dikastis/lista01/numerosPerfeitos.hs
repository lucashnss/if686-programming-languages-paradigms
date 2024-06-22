-- Um número positivo é chamado perfeito se ele é igual à soma dos quadrados de seus fatores primos. 
-- Defina a função perfeitos :: Integer -> [Integer] que, ao receber um argumento n, retorna a lista dos números 
-- perfeitos menores ou iguais a n união {1}. Para isso, defina e use função fatores, que retorna a lista de 
-- fatores primos de seu argumento. Na sua solução utilize composição de funções.

fatores :: Integer -> [Integer]
fatores n = fatoresAux n 2

fatoresAux :: Integer -> Integer -> [Integer]
fatoresAux 1 _ = []
fatoresAux n f
  | n `mod` f == 0 = f : fatoresAux (n `div` f) f
  | otherwise      = fatoresAux n (f + 1)

ehPerfeito :: Integer -> Bool
ehPerfeito n = n == sum (fatores n)

perfeitos :: Integer -> [Integer]
perfeitos n = 1 : filter ehPerfeito [1..n]

main :: IO()
main = interact $ show . perfeitos . read
