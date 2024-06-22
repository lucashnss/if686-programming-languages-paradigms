-- Defina uma função recursiva:

-- merge :: Ord a => [a] -> [a] -> [a]
-- Que une duas listas ordenadas e resulta em uma única lista ordenada.


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x: merge xs (y:ys)
  | otherwise = y: merge (x:xs) ys



main :: IO ()
main = do
  x <- getLine
  y <- getLine
  print $ merge (map (read :: String -> Int) (words x)) (map (read :: String -> Int) (words y))