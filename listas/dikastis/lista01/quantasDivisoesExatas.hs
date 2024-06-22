-- Defina função:

-- numDiv:: Integral a => a −> a −> a
-- Que recebe dois valores inteiros e retorna o número de vezes que uma divisão exata pode ser realizada, 
-- neste processo de substituir o dividendo pelo quociente. O processo de divisões sucessivas deve terminar 
-- quando uma divisão exata não existe.

-- OBS: Defina numDiv recursivamente, usando guardas.

-- Dica: use as funções mod e div.


numDiv :: Integral a => a -> a -> a 
numDiv n m 
  | n `mod` m /= 0 = 0
  | n `mod` m == 0 = 1 + numDiv (n `div` m) m



main :: IO ()
main = do
   a <- readLn
   b <- readLn
   print (numDiv (a :: Int) (b :: Int))