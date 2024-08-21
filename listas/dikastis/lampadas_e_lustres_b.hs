-- Um lustre pode ser composto por pendentes ou barras. Uma lâmpada sempre está ligada (ou associada) a um pendente. Por sua vez, uma barra pode ter dois lustres associados a ela, um à esquerda; outro, à direita. A potência de um lustre é igual à soma das potências das lâmpadas que estão nele.

-- Defina o tipo algébrico Lustre.

-- Defina uma função

-- potencia :: Lustre -> Int
-- para calcular a potência de um lustre (Soma das potências das lâmpadas do lustre)

-- Utilize a seguinte função main:

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


main = interact $ show . potencia . (read :: String -> Lustre)