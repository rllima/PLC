fatN :: Int -> [Int]
fatN a = [product [1..b] | b <- [1..a]]

testaLista :: (a -> Bool) -> [a] -> Bool
testaLista f [] = True
testaLista f (y:ys) = (f y) && (testaLista f ys)

testaListM :: (a -> Bool) -> [a] -> Bool
testaListM f [] = True
testaListM f a = and (map f a)

testaListF :: (a -> Bool) -> [a] -> Bool
testaListF f [] = True
testaListF f x= foldr (\a b -> f a && b) True x

type Nome = String
type Conteudo = String
<<<<<<< HEAD
data Arquivo = Simples Nome Conteudo | Diretorio Nome [Arquivo] deriving (Eq)

instance Show Arquivo where
  show (Simples n _) = n
  show (Diretorio n _) = n

nomeArquivo :: Arquivo -> Nome
nomeArquivo (Simples n _) = n
nomeArquivo (Diretorio n _) = n

mudarDir :: Arquivo -> Nome -> Nome
mudarDir (Simples n c) name = "Não existe"
mudarDir (Diretorio n a) name
 |n==nom = n
 |nom 'elem' [mudarDir n name] | x <- a] = nomeArquivo
 |otherwise = "Nao Existe"

allArq :: Arquivo -> [Nome]
allArq (Simples n c) = [n]
allArq (Diretorio n []) = []
allArq (Diretorio n (x:xs)) = allArq x ++ allArq (Diretorio n xs)
=======
>>>>>>> 2209f26a6b3d8fd95360c49b93aac2339d36dc0e
