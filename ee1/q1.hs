-- 1.
-- a)
type Pessoa = String 
type Equipe = [Pessoa]
type Identificador = String
type Projeto = (Identificador, Equipe)
-- o tipo Projetos representa a associação entre projetos e equipes
type Projetos = [Projeto] 

-- b)
criaProjeto :: Identificador -> Pessoa -> Projetos -> Projetos 
criaProjeto i p [] = [(i,[p])]
criaProjeto i p (x:xs)
    | i == fst x = x:xs
    | otherwise = x: criaProjeto i p xs

-- c)
equipe :: Identificador -> Projetos -> Equipe
equipe i [] = []
equipe i (x:xs)
    | i == fst x = snd x 
    | otherwise = equipe i xs

-- d)
naEquipe :: Identificador -> Pessoa -> Projetos -> Bool
naEquipe i p [] = False 
naEquipe i p (x:xs)
    | (i == fst x) && elem p (snd x)= True
    | (i == fst x) && not(elem p (snd x)) = False
    | otherwise = naEquipe i p xs 

-- e)
acrescentarPessoa :: Identificador -> Pessoa -> Projetos -> Projetos 
acrescentarPessoa i p [] = []
acrescentarPessoa i p (x:xs)
    | (i == fst x) && elem p (snd x) = (x:xs)
    | i == fst x = (fst x,snd x ++ [p]):xs
    | otherwise = x:acrescentarPessoa i p xs