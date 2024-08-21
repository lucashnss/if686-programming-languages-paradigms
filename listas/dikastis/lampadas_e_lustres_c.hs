-- Defina uma função

-- balanceado :: Lustre -> Bool
-- que determina se um lustre está balanceado.

-- Um lustre com apenas uma lâmpada é considerado balanceado. Se os lustres das extremidades forem balanceados e as potências de suas lâmpadas forem iguais, esse lustre é considerado balanceado.

-- Utilize a seguinte função main:

-- main = interact $ show . balanceado . (read :: String -> Lustre)

data Lampada = Compacta String Int | 
               Incandescente String Int
              deriving(Read)

instance Show Lampada where
  show (Compacta nome potencia) = "Compacta " ++ nome ++ " " ++ show potencia
  show (Incandescente nome potencia) = "Incandescente " ++ nome ++ " " ++ show potencia

data Lustre = Pendente Lampada | Barra Lustre Lustre deriving (Show, Read)

potencia :: Lustre -> Int 
potencia (Pendente (Compacta _ p)) = p  
potencia (Pendente (Incandescente _ p)) = p 
potencia (Barra l1 l2) = potencia l1 + potencia l2

balanceado :: Lustre -> Bool
balanceado (Pendente _) = True
balanceado (Barra l1 l2) = balanceado l1 && balanceado l2 && potencia l1 == potencia l2

main = interact $ show . balanceado . (read :: String -> Lustre)
