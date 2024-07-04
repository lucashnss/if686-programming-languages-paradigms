--  a)
data Nat = Zero | Succ Nat deriving(Eq,Show)

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n   
    | n > 0 = Succ (int2Nat(n-1))
    | otherwise = error "Número inválido"
    

-- b)
nat2Int :: Nat -> Int 
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int n

-- c)
somaNat :: Nat -> Nat -> Nat
somaNat Zero n = n 
somaNat (Succ n) m = Succ (somaNat n m)

-- d)
somaInt :: Nat -> Nat -> Int 
somaInt n m = nat2Int(somaNat n m)