module Datas_types where
import Prelude hiding (head,tail,null,sum,concat,(++),elem,zip)    
import Data.Char
import Language.Haskell.TH (Strict)

minAndMax :: Int -> Int -> (Int,Int)
minAndMax a b 
  | a >= b = (a,b)
  | otherwise = (b,a)

addPair :: (Int,Int) -> Int
addPair (0,y) = y
addPair (x,y) = x+y
 
maddPair :: (Int,Int) -> Int
maddPair p = fst p + snd p

fibStep :: (Integer,Integer) -> (Integer, Integer)
fibStep  (u,v) = (v,u+v)

fibPair :: Integer -> (Integer,Integer)
fibPair n 
  | n == 0 = (0,1)
  | otherwise = fibStep (fibPair(n-1))

fastFib :: Integer -> Integer
fastFib n = fst (fibPair n)

data Move = Rock | Scissor | Paper deriving(Eq,Show)

score :: Move -> Move ->  Int
score Rock Rock = 0
score Rock Paper = -1
score Rock Scissor = 1
score Paper Rock = 1
score Paper Scissor = -1
score Paper Paper = 0
score Scissor Rock = -1
score Scissor Paper = 1
score Scissor Scissor = 0

type Name = String
type Age = Int
data People = Person Name Age deriving(Eq,Show)

showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n

{- Calculating second degree equations -}

oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = -(b/(2*a))

twoRoot :: Float -> Float -> Float -> (Float,Float)
twoRoot a b c = (e+d,e-d) 
  where
    e = (-b) / (2 * a)
    d = sqrt (b^2 - (4 * a * c)) / (2 * a)

roots :: Float -> Float -> Float -> String
roots a b c
  | delta > 0   = show f ++ "," ++ show s
  | delta == 0  = show (oneRoot a b c )
  | otherwise   = "no roots"
  where
    delta = b^2 - (4 * a * c)
    (f,s) = twoRoot a b c 
  
addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ m+n | (m,n) <- pairList]

isDigit :: Char -> Bool
isDigit ch = (ch >= '0') && (ch <= '9')

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

allEven :: [Int] -> Bool
allEven xs = xs == [x | x <- xs , isEven x]

allOdd :: [Int] -> Bool
allOdd xs = [] == [x | x <- xs , isEven x]

first :: (a,b) -> a 
first (x,y) = x 

mystery :: Int -> Int -> Int
mystery 0 y = y
mystery x _ = x

{- lists -}

head :: [a] -> a 
head (x:_) = x  

tail :: [a] -> [a]
tail (_:xs) = xs

null :: [a] -> Bool
null []     = True
null (_:_)  = False

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

concat :: [[a]] -> [a] 
concat [] = []
concat (x:xs) = x ++ concat xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

elem :: Int -> [Int] -> Bool
elem x [] = False
elem x (y:ys) = x==y || elem x ys

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = (x*2):doubleAll xs

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
  | x <= y = x:(y:ys)
  | otherwise =  y: ins x ys

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort[y|y <- xs, y<=x] ++ [x] ++ qSort[y|y <- xs, y>x]