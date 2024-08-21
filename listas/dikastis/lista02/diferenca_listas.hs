-- Descubra como duas listas diferem uma da outra.

-- Se tiverem comprimentos diferentes, devolva Just "<comprimento da lista> /= <comprimento de outra lista>"

-- Se tiverem o mesmo comprimento, você deve encontrar o primeiro índice i para o qual os os elementos diferem, e o retorno será Just "<valor no indice i> /= < outro valor no indice i>",

-- Se as listas forem as mesmas, devolver Nothing

-- Escreva a assinatura de tipo para findDifference.

findDifference :: (Eq a, Show a) => [a] -> [a] -> Maybe String
findDifference xs ys
  | length xs /= length ys = Just (show (length xs) ++ " /= " ++ show (length ys))
  | otherwise = findDiff xs ys 
  where
    findDiff [] [] = Nothing
    findDiff (x:xs) (y:ys)
      | x /= y = Just (show x ++ " /= " ++ show y)
      | otherwise = findDiff xs ys


main = do
  x <- getLine
  y <- getLine
  print $ findDifference (map (read :: String -> Int) (words x)) (map (read :: String -> Int) (words y))