module Program where
import Prelude hiding (max)
import Data.Char
import GHC.RTS.Flags (GCFlags(squeezeUpdFrames))

tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x == y) && (y == z)

exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

eXor :: Bool -> Bool -> Bool
eXor True x = not x
eXor False x = x

mNot :: Bool -> Bool
mNot True = False
mNot False = True

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

maiuscula :: Char -> Char
maiuscula ch = toEnum (fromEnum ch + offset) 
 
max :: Int -> Int -> Int
max x y 
  | x >= y = x
  | otherwise = y

maxTres :: Int -> Int -> Int -> Int
maxTres x y z 
  | x >= y && x >= z = x
  | y >= z = y
  | otherwise = z

max':: Int -> Int -> Int
max' x y = if x >= y then x else y    

compareMax :: Int -> Int -> Bool
compareMax x y = max x y == max' x y


nextAnswer :: Int
answer :: Int
answer = 42; nextAnswer = 50

cube :: Int -> Int
cube n = n*n*n

onThreeLines ::  String -> String -> String -> String
onThreeLines a b c = a ++ b ++ c 

romanDigit :: Char -> String
romanDigit ch 
  | ch  == '1' = "I"
  | ch  == '2' = "II"
  | ch  == '3' = "III"
  | ch  == '4' = "IV"
  | ch  == '5' = "V"
  | ch  == '6' = "VI"
  | ch  == '7' = "VII"
  | ch  == '8' = "VIII"
  | ch  == '9' = "IV"

square :: Int -> Int
square n = n*n

allEqual :: Int -> Int -> Int -> Bool
allEqual x y z = (x == y) && (y == z) 

infixr 7 &&&
(&&&) :: Int -> Int -> Int
x &&& y
  | x > y = y
  | otherwise = x

-- triangle area with its sides
triArea :: Float -> Float -> Float -> Float 
triArea a b c 
  | possible = sqrt(s*(s-a)*(s-b)*(s-c))
  | otherwise = 0
  where 
    s = (a+b+c)/2
    possible = ((a+b) > c) && ((a+c) > b) && ((b+c) > a)

sumSquares :: Int -> Int -> Int
sumSquares n m 
  = sqN + sqM 
  where 
    sqN = n*n
    sqM = m*m

isOdd :: Int -> Bool
isOdd n 
  | n < 0 = False
  | otherwise = isEven (n-1)

isEven :: Int -> Bool
isEven n 
  | n < 0 = False
  | n == 0 = True
  | otherwise = isOdd (n-1)

msumSquares :: Int -> Int -> Int 
msumSquares x y = let sqX = x * x 
                      sqY = y * y
                  in sqX + sqY

fat :: Int -> Int
fat n 
  | n == 0 = 1 
  | n > 0 = fat (n-1) * n 
  | otherwise = error "fac only defined on natural numbers" 

power3 :: Int -> Int 
power3 n 
  | n == 0 = 1
  | n > 0 = power3 (n-1) * 3

sumFacs :: Int -> Int
sumFacs n 
  | n == 0 = 1  
  | n > 0 = sumFacs (n-1) + fat n

sumFun :: (Int -> Int) -> Int -> Int
sumFun f n  
  | n == 0 = f 0
  | n > 0 = sumFun f (n-1) + f n 

fib :: Int -> Int
fib n 
  | n == 0 = 0
  | n == 1 = 1
  | n > 0 = fib(n-2) + fib(n-1)

resto :: Int -> Int -> Int
resto m n 
  | m < n = m
  | otherwise = resto(m-n) n

mdiv :: Int -> Int -> Int
mdiv m n 
  | m < n = 0
  | otherwise = 1 + mdiv (m-n) n

vendas :: Int -> Int
vendas n 
  | n == 0 = 13
  | n == 1 = 54
  | n == 2 = 19
  | n == 3 = 30
  | n == 4 = 70

totalVendas :: Int -> Int
totalVendas n 
 | n == 0 = vendas 0
 | otherwise = totalVendas (n-1) + vendas n

maxVendas :: Int -> Int
maxVendas n
  | n == 0 = vendas 0
  | otherwise = max' (maxVendas(n-1)) (vendas n)
  