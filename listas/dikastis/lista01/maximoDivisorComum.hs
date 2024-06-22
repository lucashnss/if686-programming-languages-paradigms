-- Defina uma função recursiva para calcular o máximo divisor comum de dois números inteiros não negativos a e b, 
-- usando o algoritmo de Euclides.
mdc :: Int -> Int -> Int
mdc a b 
  | b == 0 = a 
  | otherwise = mdc b (a `mod` b)



main :: IO ()
main = do
   a <- readLn
   b <- readLn
   print (mdc (a :: Int) (b :: Int))