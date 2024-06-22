-- Defina uma função expNE que calcule o resultado da exponenciação inteira de X elevado à Y, sem recorrer a funções pré-definidas.
expNE :: Float -> Int -> Float 
expNE n k 
  | k == 0 = 1
  | k > 0 = n * expNE n (k-1)

main :: IO ()
main = do
   x <- readLn
   y <- readLn
   print (expNE (x :: Float) (y :: Int))