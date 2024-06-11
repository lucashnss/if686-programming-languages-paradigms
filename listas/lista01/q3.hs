import Prelude hiding (min)
infixl 4 !=
(!=) :: Int -> Int -> Bool
x != y = not (x == y)


threeDiferent :: Int -> Int -> Int -> Bool
threeDiferent x y z = (x != y) && (y != z) && (x != z)

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual x y z w = (x == y) && (y == z) && (z == w)

min :: Int -> Int -> Int
min x y 
  | x <= y = x
  | otherwise = y

minThree :: Int -> Int -> Int -> Int
minThree x y z 
  | x <= y && x <= z = x
  | y <= z = y
  | otherwise = z

mystery :: Int -> Int -> Int -> Bool
mystery m n p  = not ((m==n) && (n==p))

{- maxThree (4+5) (2+6) (100 `div` 7)
  -> maxThree 9 8 14
  -> 9 >=8 && 9>>14
  -> True && False
  -> False
  -> 8 >= 14
  -> False
  -> otherwise
  -> false 
-}

