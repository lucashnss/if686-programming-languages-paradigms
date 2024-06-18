import Data.List(group,sort)

import Data.Maybe (Maybe(Nothing))
import Control.Arrow (ArrowChoice(left, right))

findFirstDifference :: (Eq a,Show a) => [a] -> [a]  -> Maybe String
findFirstDifference [] [] = Nothing
findFirstDifference (x:xs) (y:ys)
    | x /= y = Just $ show x ++ "/=" ++ show y
    | otherwise = findFirstDifference xs ys

findDifference :: (Eq a,Show a) => [a] -> [a]  -> Maybe String
findDifferente [] [] = Nothing
findDifference xs ys
    | length xs /= length ys = Just $ show (length xs) ++ "/=" ++ show (length ys)
    | otherwise = findFirstDifference xs ys

data Vetor = Vetor Integer Integer Integer
    deriving Show 

instance Eq Vetor where
    (Vetor x1 y1 z1) == (Vetor x2 y2 z2) = (x1 == x2) && (y1 == y2) && (z1==z2)

instance Num Vetor where
    (+) :: Vetor -> Vetor -> Vetor
    (Vetor x1 y1 z1) + (Vetor x2 y2 z2) = Vetor (x1+x2) (y1+y2) (z1+z2)

    (-) :: Vetor -> Vetor -> Vetor
    (Vetor x1 y1 z1) - (Vetor x2 y2 z2) = Vetor (x1-x2) (y1-y2) (z1-z2)

    (*) :: Vetor -> Vetor -> Vetor
    (Vetor x1 y1 z1) * (Vetor x2 y2 z2) = Vetor (x1*x2) (y1*y2) (z1*z2)

    abs :: Vetor -> Vetor
    abs(Vetor x y z) = Vetor (abs x) (abs y) (abs z)

    signum :: Vetor -> Vetor
    signum(Vetor x y z) = Vetor (signum x) (signum y) (signum z)

    negate :: Vetor -> Vetor
    negate(Vetor x y z) = Vetor (negate x) (negate y) (negate z)

    fromInteger :: Integer -> Vetor
    fromInteger n = Vetor n n n 

countElements :: [a] -> (Int,a)
countElements g = (length g,head g) 

freqs :: (Eq a, Ord a) => [a] -> [(Int,a)]
freqs xs = map countElements . group . sort $ xs


data ITree = ILeaf | Node Int ITree ITree 
     deriving (Show)

instance Eq ITree where
    (==) :: ITree -> ITree -> Bool
    ILeaf == ILeaf = True   
    Node x left1 right1 == Node y left2 right2 =
        (x == y) && (left1 == left2) && (right1 == right2)
    _ == _ = False