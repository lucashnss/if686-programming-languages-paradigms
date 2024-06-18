import Data.List(group,sort)

data Vetor = Vetor Integer Integer Integer
    deriving Show 

instance Eq Vetor where
    (Vetor v1 v2 v3) == (Vetor v4 v5 v6) = (v1 == v4) && (v2 == v5) && (v3 == v6) 

instance Num Vetor where
    (Vetor v1 v2 v3) + (Vetor v4 v5 v6) = Vetor (v1+v4) (v2+v5) (v3+v6)
    (Vetor v1 v2 v3) - (Vetor v4 v5 v6) = Vetor (v1-v4) (v2-v5) (v3-v6)
    (Vetor v1 v2 v3) * (Vetor v4 v5 v6) = Vetor (v1*v4) (v2*v5) (v3*v6)
    abs(Vetor v1 v2 v3) = Vetor (abs(v1)) (abs(v2)) (abs(v3))
    signum(Vetor v1 v2 v3) = Vetor (signum(v1)) (signum(v2)) (signum(v3))
    negate(Vetor v1 v2 v3) = Vetor (negate(v1)) (negate(v2)) (negate(v3))

countElements :: [a] -> (Int,a)
countElements g = (length g,head g) 

freqs :: (Eq a, Ord a) => [a] -> [(Int,a)]
freqs xs = map countElements . group . sort $ xs


-- data ITree = ILeaf | Node Int ITree ITree 
--     deriving (Show)

-- instance Eq ITree where
--     ITree a ILeaf ILeaf == ITree b ILeaf ILeaf = a == b 