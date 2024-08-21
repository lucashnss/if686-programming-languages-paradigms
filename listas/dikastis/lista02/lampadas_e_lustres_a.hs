-- Uma lâmpada é caracterizada por ser compacta ou incandescente. Além, disso, toda lâmpada possui o nome do seu fabricante e a potência como um valor em Watts.

-- Defina o tipo algébrico Lampada, de acordo com as características descritas.

-- Estabeleça que exibir uma lâmpada resulta em uma string que começa com a palavra "Compacta", no caso de lâmpada compacta, ou com a string "Incandescente". Estas strings são seguidas do nome do fabricante e da potência da lâmpada, todos os itens separados por apenas um espaço. Ou seja, defina que o tipo Lampada é instância da classe Show.

-- Utilize a seguinte função main:

-- main = interact $ show . (read :: String -> Lampada)
-- OBS.: É necessário que você declare a instância de Show, ou seja, como Lampada se comporta quando usada junto ao Show. Contudo, também é necessário que ela derive Read, para que possa ser lida do terminal. Não é necessário implementar a instância de Read, apenas derivar.

data Lampada = Compacta String Int | 
               Incandescente String Int
              deriving(Read)

instance Show Lampada where
  show (Compacta nome potencia) = "Compacta " ++ nome ++ " " ++ show potencia
  show (Incandescente nome potencia) = "Incandescente " ++ nome ++ " " ++ show potencia

main :: IO()
main = interact $ show . (read :: String -> Lampada)