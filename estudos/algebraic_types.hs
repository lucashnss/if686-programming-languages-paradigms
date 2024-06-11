import Language.Haskell.TH (Exp)
import Prelude hiding(Either,Left,Right)

data Expr = Lit Integer|
            Add Expr Expr|
            Sub Expr Expr

data NTree = NilT | Node Integer NTree NTree

eval :: Expr -> Integer
eval (Lit n) = n 
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Sub e1 e2) = showExpr e1 ++ "-" ++ showExpr e2

sumTree :: NTree -> Integer
sumTree NilT = 0
sumTree (Node n tl tr) = n + sumTree tl + sumTree tr

depth :: NTree -> Integer
depth NilT = 0
depth (Node n tl tr) = 1 + max (depth tl) (depth tr)

occurs :: NTree -> Integer -> Integer
occurs NilT p = 0
occurs (Node n tl tr) p 
    | n == p = 1 + occurs tl p + occurs tr p
    | otherwise = occurs tl p + occurs tr p

data Tree a = Nil | NodeA a (Tree a) (Tree a)
    deriving (Eq,Ord,Show,Read)

mapTree :: (a -> b) -> Tree a -> Tree b 
mapTree f Nil = Nil 
map f (NodeA x tl tr) = NodeA (f x) (mapTree f tl) (mapTree f tr)

collapse :: Tree a -> [a]
collapse Nil = []
collapse (NodeA x tl tr) = collapse tl ++ [x] ++ collapse tr

data Either a b = Left a | Right b 
            deriving(Eq,Ord,Show,Read)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

data Maybe a = Nothing | Just a
            deriving(Eq,Ord,Show,Read)